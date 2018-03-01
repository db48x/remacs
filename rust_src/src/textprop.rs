//! Text property support

use std::ptr;

use remacs_macros::lisp_fn;
use remacs_sys::{EmacsInt, Lisp_Object, Qnil};
use remacs_sys::get_char_property_and_overlay;

use buffers::LispBufferRef;
use lisp::{LispObject, LispCons};
use lisp::defsubr;
use symbols::LispSymbolRef;

/// Return the value of POSITION's property PROP, in OBJECT.
/// Both overlay properties and text properties are checked.
/// OBJECT is optional and defaults to the current buffer.
/// If POSITION is at the end of OBJECT, the value is nil.
/// If OBJECT is a buffer, then overlay properties are considered as well as
/// text properties.
/// If OBJECT is a window, then that window's buffer is used, but window-specific
/// overlays are considered only if they are associated with OBJECT.
#[lisp_fn(min = "2")]
pub fn get_char_property(position: EmacsInt, prop: LispObject, object: LispObject) -> LispObject {
    LispObject::from_raw(unsafe {
        get_char_property_and_overlay(
            LispObject::from(position).to_raw(),
            prop.to_raw(),
            object.to_raw(),
            ptr::null_mut(),
        )
    })
}

/// Like `get-char-property', but with extra overlay information.
/// The value is a cons cell.  Its car is the return value of `get-char-property'
/// with the same arguments--that is, the value of POSITION's property
/// PROP in OBJECT.  Its cdr is the overlay in which the property was
/// found, or nil, if it was found as a text property or not found at all.
///
/// OBJECT is optional and defaults to the current buffer.  OBJECT may be
/// a string, a buffer or a window.  For strings, the cdr of the return
/// value is always nil, since strings do not have overlays.  If OBJECT is
/// a window, then that window's buffer is used, but window-specific
/// overlays are considered only if they are associated with OBJECT.  If
/// POSITION is at the end of OBJECT, both car and cdr are nil.
#[lisp_fn(name="get-char-property-and-overlay")]
pub fn rust_get_char_property_and_overlay(position: EmacsInt, prop: LispSymbolRef, buffer: Option<LispBufferRef>) -> LispCons
{
    let mut overlay: Lisp_Object = Qnil;
    let property: Lisp_Object = unsafe {
        get_char_property_and_overlay(LispObject::from(position).to_raw(),
                                      LispObject::from(prop).to_raw(),
                                      LispObject::from(buffer).to_raw(),
                                      &mut overlay)
    };
    LispObject::cons(LispObject::from_raw(property), LispObject::from_raw(overlay)).as_cons_or_error()
}


include!(concat!(env!("OUT_DIR"), "/textprop_exports.rs"));
