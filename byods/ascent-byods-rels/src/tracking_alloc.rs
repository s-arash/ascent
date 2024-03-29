use std::alloc::GlobalAlloc;


/// An allocator that keeps track of the currently-allocated memory
/// (in globals `MAX_ALLOC` and `CURRENT_ALLOC`)
#[derive(Default)]
pub struct TrackingAllocator<Alloc = std::alloc::System>(pub Alloc);

impl TrackingAllocator<std::alloc::System> {
   pub const fn new() -> Self {
      Self(std::alloc::System)
   }
}

pub static mut MAX_ALLOC: usize = 0;
pub static mut CURRENT_ALLOC: usize = 0;

unsafe impl<T: GlobalAlloc> GlobalAlloc for TrackingAllocator<T> {
   
   #[inline]
   unsafe fn alloc(&self, layout: std::alloc::Layout) -> *mut u8 {
      CURRENT_ALLOC += layout.size();
      MAX_ALLOC = MAX_ALLOC.max(CURRENT_ALLOC);
      self.0.alloc(layout)
   }

   #[inline]
   unsafe fn dealloc(&self, ptr: *mut u8, layout: std::alloc::Layout) {
      CURRENT_ALLOC = CURRENT_ALLOC.saturating_sub(layout.size());
      self.0.dealloc(ptr, layout)
   }

   #[inline]
   unsafe fn alloc_zeroed(&self, layout: std::alloc::Layout) -> *mut u8 {
      CURRENT_ALLOC += layout.size();
      MAX_ALLOC = MAX_ALLOC.max(CURRENT_ALLOC);
      self.0.alloc_zeroed(layout)
   }

   #[inline]
   unsafe fn realloc(&self, ptr: *mut u8, layout: std::alloc::Layout, new_size: usize) -> *mut u8 {
      if new_size > layout.size() {
         CURRENT_ALLOC += new_size - layout.size();
         MAX_ALLOC = MAX_ALLOC.max(CURRENT_ALLOC);
      } else {
         CURRENT_ALLOC = CURRENT_ALLOC.saturating_sub(layout.size() - new_size);
      }
      self.0.realloc(ptr, layout, new_size)
   }
}

pub fn current_alloc() -> usize {
   unsafe { CURRENT_ALLOC }
}

pub fn max_alloc() -> usize {
   unsafe { MAX_ALLOC }
}

pub fn reset_max_alloc() {
   unsafe {
      MAX_ALLOC = CURRENT_ALLOC;
   }
}