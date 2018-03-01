use std::ptr;
use libc::ptrdiff_t;

use buffers::LispOverlayRef;
use symbols::LispSymbolRef;
use threads::ThreadState;
use lisp::LispObject;
use textprop::{get_char_property, rust_get_char_property_and_overlay};

use remacs_sys::{Fprevious_char_property_change};
use remacs_sys::{EmacsInt, intervals_equal, Lisp_Object, interval, temp_set_point_both, find_interval, previous_interval, bset_point_before_scroll, buffer_has_overlays, buffer_intervals, buf_charpos_to_bytepos, textget, text_property_stickiness, invisible_prop, globals};
use remacs_sys::{Qnil, Qinvisible, Qintangible, Qpoint_left, Qpoint_entered};

/// Set point in BUFFER to CHARPOS.  If the target position is
/// before an intangible character, move to an ok place.
#[no_mangle]
pub extern "C" fn set_point(charpos: ptrdiff_t) {
    set_point_both(charpos,
                   unsafe {
                       buf_charpos_to_bytepos(ThreadState::current_buffer().as_mut(), charpos)
                   });
}

fn text_prop_means_invisible(prop: LispObject) -> bool {
    let invis_spec = ThreadState::current_buffer().invisibility_spec;
    if bool::from(LispObject::from_raw(invis_spec)) {
        prop.is_not_nil()
    } else {
        unsafe { invisible_prop(prop.to_raw(), invis_spec) != 0 }
    }
}

/// If there's an invisible character at position POS + TEST_OFFS in the
/// current buffer, and the invisible property has a `stickiness' such that
/// inserting a character at position POS would inherit the property it,
/// return POS + ADJ, otherwise return POS.  If TEST_INTANG, intangibility
/// is required as well as invisibility.
///
/// TEST_OFFS should be either 0 or -1, and ADJ should be either 1 or -1.
///
/// Note that `stickiness' is determined by overlay marker insertion types,
/// if the invisible property comes from an overlay.
fn adjust_for_invis_intang(pos: ptrdiff_t, test_offs: ptrdiff_t, adj: ptrdiff_t, test_intang: bool) -> ptrdiff_t
{
    let current_buffer = ThreadState::current_buffer();

    if (adj < 0 && pos + adj < current_buffer.begv) || (adj > 0 && pos + adj > current_buffer.zv()) {
        /* POS + ADJ would be beyond the buffer bounds, so do no adjustment.  */
        return pos;
    }

    let test_pos = (pos + test_offs) as EmacsInt;
    let (invis_propval, invis_overlay) = rust_get_char_property_and_overlay(test_pos, LispObject::from_raw(Qinvisible).as_symbol_or_error(), LispObject::from_raw(Qnil).as_buffer()).as_tuple();
    let is_intangible = bool::from(get_char_property(test_pos, LispObject::from_raw(Qintangible), LispObject::constant_nil()) );
    /* This next test is true if the invisible property has a stickiness
       such that an insertion at POS would inherit it.  */
    let is_invisible = if let Some(overlay) = invis_overlay.as_overlay() {
        /* Invisible property is from an overlay.  */
        if test_offs == 0 {
            overlay.start().as_marker_or_error().insertion_type() == false
        } else {
            overlay.end().as_marker_or_error().insertion_type() == true
        }
    } else {
        /* Invisible property is from a text-property.  */
        let stickiness = unsafe { text_property_stickiness(Qinvisible, LispObject::from(pos).to_raw(), Qnil) };
        if test_offs == 0 {
            stickiness == 1
        } else {
            stickiness == -1
        }
    };

    if (!test_intang || is_intangible) && text_prop_means_invisible(invis_propval) && is_invisible {
        pos + adj
    } else {
        pos
    }
}

/// Set point in BUFFER to CHARPOS, which corresponds to byte
/// position BYTEPOS.  If the target position is
/// before an intangible character, move to an ok place.
#[no_mangle]
pub extern "C" fn set_point_both(mut charpos: ptrdiff_t, mut bytepos: ptrdiff_t)
{
    let mut current_buffer = ThreadState::current_buffer();
    let mut to: *mut interval;
    let mut from: *mut interval;
    let mut toprev: *mut interval;
    let fromprev: *mut interval;
    let old_position: ptrdiff_t = current_buffer.pt();
    /* This ensures that we move forward past intangible text when the
       initial position is the same as the destination, in the rare
       instances where this is important, e.g. in line-move-finish
       (simple.el).  */
    let backwards = charpos < old_position;

    unsafe {
        bset_point_before_scroll(current_buffer.as_mut(), Qnil);
    }

    if charpos == current_buffer.pt() {
        return;
    }

    /* In a single-byte buffer, the two positions must be equal.  */
    assert!(current_buffer.zv() != current_buffer.zv_byte || charpos == bytepos);

    /* Check this now, before checking if the buffer has any intervals.
       That way, we can catch conditions which break this sanity check
       whether or not there are intervals in the buffer.  */
    assert!(charpos <= current_buffer.zv() && charpos >= current_buffer.begv);

    let have_overlays = unsafe { buffer_has_overlays() };
    //let buffer_intervals = unsafe { buffer_intervals(current_buffer.as_mut()) };

    /* If we have no text properties and overlays,
       then we can do it quickly.  */
    if unsafe { buffer_intervals(current_buffer.as_mut()) }.is_null() && ! have_overlays {
        unsafe { temp_set_point_both(current_buffer.as_mut(), charpos, bytepos) };
        return;
    }

    /* Set TO to the interval containing the char after CHARPOS,
       and TOPREV to the interval containing the char before CHARPOS.
       Either one may be null.  They may be equal.  */
    to = unsafe { find_interval(unsafe { buffer_intervals(current_buffer.as_mut()) }, charpos) };
    if charpos == current_buffer.begv {
        toprev = ptr::null_mut()
    }
    else if !to.is_null() && unsafe { (*to).position } == charpos {
        toprev = unsafe { previous_interval(to) };
    }
    else {
        toprev = to;
    }

    let buffer_point = if current_buffer.pt() == current_buffer.zv() { current_buffer.zv() - 1 } else { current_buffer.pt() };

    /* Set FROM to the interval containing the char after current_buffer.pt(),
       and FROMPREV to the interval containing the char before current_buffer.pt().
       Either one may be null.  They may be equal.  */
    /* We could cache this and save time.  */
    from = unsafe { find_interval(unsafe { buffer_intervals(current_buffer.as_mut()) }, buffer_point) };
    if buffer_point == current_buffer.begv {
        fromprev = ptr::null_mut();
    }
    else if !from.is_null() && unsafe { (*from).position } == current_buffer.pt() {
        fromprev = unsafe { previous_interval(from) };
    }
    else if buffer_point != current_buffer.pt() {
        fromprev = from;
        from = ptr::null_mut();
    }
    else {
        fromprev = from;
    }

    let to_is_visible = !to.is_null() && !bool::from(LispObject::from_raw(unsafe { textget((*to).plist, Qinvisible) }));
    /* Moving within an interval. */
    if to == from && toprev == fromprev && to_is_visible && !have_overlays {
        unsafe { temp_set_point_both(current_buffer.as_mut(), charpos, bytepos) };
        return;
    }

    let original_position = charpos;

    /* If the new position is between two intangible characters
       with the same intangible property value,
       move forward or backward until a change in that property.  */
    let inhibited = bool::from(LispObject::from_raw(unsafe { globals.f_Vinhibit_point_motion_hooks }));
    if !inhibited && ((!to.is_null() && !toprev.is_null()) || have_overlays
      /* Intangibility never stops us from positioning at the beginning
         or end of the buffer, so don't bother checking in that case.  */
      && charpos != current_buffer.begv && charpos != current_buffer.zv())
    {
        let mut pos: ptrdiff_t;
        let intangible_propval: LispObject;

        if backwards {
            /* If the preceding character is both intangible and invisible,
               and the invisible property is `rear-sticky', perturb it so
               that the search starts one character earlier -- this ensures
               that point can never move to the end of an invisible/
               intangible/rear-sticky region.  */
            charpos = adjust_for_invis_intang(charpos, -1, -1, true);
            pos = charpos;

            /* If following char is intangible,
               skip back over all chars with matching intangible property.  */
            intangible_propval = get_char_property(pos as EmacsInt, LispObject::from_raw(Qintangible), LispObject::constant_nil());

            if !bool::from(intangible_propval) {
                while pos > current_buffer.begv &&
                      get_char_property(pos as EmacsInt - 1, LispObject::from_raw(Qintangible), LispObject::constant_nil()) == intangible_propval
                {
                    pos = LispObject::from_raw(unsafe { Fprevious_char_property_change(LispObject::from(pos).to_raw(), Qnil) }).as_fixnum_or_error() as isize;

                    /* Set CHARPOS from POS, and if the final intangible character
                       that we skipped over is also invisible, and the invisible
                       property is `front-sticky', perturb it to be one character
                       earlier -- this ensures that point can never move to the
                       beginning of an invisible/intangible/front-sticky region.  */
                    charpos = adjust_for_invis_intang(pos, 0, -1, false);
                }
            }
        } else {
            /* If the following character is both intangible and invisible,
               and the invisible property is `front-sticky', perturb it so
               that the search starts one character later -- this ensures
               that point can never move to the beginning of an
               invisible/intangible/front-sticky region.  */
            charpos = adjust_for_invis_intang(charpos, -1, -1, true);
            pos = charpos;

            /* If preceding char is intangible,
               skip forward over all chars with matching intangible property.  */
            intangible_propval = get_char_property(pos as EmacsInt - 1, LispObject::from_raw(Qintangible), LispObject::constant_nil());

            if !bool::from(intangible_propval) {
                while pos > current_buffer.begv &&
                      get_char_property(pos as EmacsInt, LispObject::from_raw(Qintangible), LispObject::constant_nil()) == intangible_propval
                {
                    pos = LispObject::from_raw(unsafe { Fprevious_char_property_change(LispObject::from(pos).to_raw(), Qnil) }).as_fixnum_or_error() as isize;

                    /* Set CHARPOS from POS, and if the final intangible character
                       that we skipped over is also invisible, and the invisible
                       property is `rear-sticky', perturb it to be one character
                       later -- this ensures that point can never move to the
                       end of an invisible/intangible/rear-sticky region.  */
                    charpos = adjust_for_invis_intang(pos, 0, -1, false);
                }
            }
        }

        bytepos = unsafe { buf_charpos_to_bytepos(current_buffer.as_mut(), charpos) };
    }

    if charpos != original_position {
        /* Set TO to the interval containing the char after CHARPOS,
           and TOPREV to the interval containing the char before CHARPOS.
           Either one may be null.  They may be equal.  */
        to = unsafe { find_interval(unsafe { buffer_intervals(current_buffer.as_mut()) }, charpos) };
        if charpos == current_buffer.begv {
            toprev = ptr::null_mut();
        }
        else if !to.is_null() && unsafe { (*to).position } == charpos {
            toprev = unsafe { previous_interval(to) };
        }
        else {
            toprev = to;
        }
    }

    /* Here TO is the interval after the stopping point
       and TOPREV is the interval before the stopping point.
       One or the other may be null.  */
    unsafe { temp_set_point_both(current_buffer.as_mut(), charpos, bytepos) };

    /* We run point-left and point-entered hooks here, if the
       two intervals are not equivalent.  These hooks take
       (old_point, new_point) as arguments.  */
    if !inhibited &&
       (!unsafe { intervals_equal(from, to) } ||
        !unsafe { intervals_equal(fromprev, toprev) })
    {
        let leave_after: Option<LispObject>;
        let leave_before: Option<LispObject>;
        let enter_after: Option<LispObject>;
        let enter_before: Option<LispObject>;

        if !fromprev.is_null() {
            leave_before = Some(LispObject::from_raw(unsafe { textget((*fromprev).plist, Qpoint_left) }));
        } else {
            leave_before = None;
        }

        if !from.is_null() {
            leave_after = Some(LispObject::from_raw(unsafe { textget((*from).plist, Qpoint_left) }));
        } else {
            leave_after = None;
        }

        if !toprev.is_null() {
            enter_before = Some(LispObject::from_raw(unsafe { textget((*toprev).plist, Qpoint_entered) }));
        } else {
            enter_before = None;
        }

        if !to.is_null() {
            enter_after = Some(LispObject::from_raw(unsafe { textget((*to).plist, Qpoint_entered) }));
        } else {
            enter_after = None;
        }

        if leave_before != enter_before {
            if let Some(leave_before) = leave_before {
                call!(leave_before,
                      LispObject::from(old_position),
                      LispObject::from(charpos));
            }
        }
        if leave_after != enter_after {
            if let Some(leave_after) = leave_after {
                call!(leave_after,
                      LispObject::from(old_position),
                      LispObject::from(charpos));
            }
        }
        if enter_before != leave_before {
            if let Some(enter_before) = enter_before {
                call!(enter_before,
                      LispObject::from(old_position),
                      LispObject::from(charpos));
            }
        }
        if enter_after != leave_after {
            if let Some(enter_after) = enter_after {
                call!(enter_after,
                      LispObject::from(old_position),
                      LispObject::from(charpos));
            }
        }
    }
}

