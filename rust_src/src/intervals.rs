use threads::ThreadState;
use libc::ptrdiff_t;
use remacs_sys::{set_point_both, buf_charpos_to_bytepos};

/// Set point in BUFFER to CHARPOS.  If the target position is
/// before an intangible character, move to an ok place.
#[no_mangle]
pub extern "C" fn set_point(charpos: ptrdiff_t) {
    unsafe {
        set_point_both(charpos, buf_charpos_to_bytepos(ThreadState::current_buffer().as_mut(), charpos));
    }
}
