use std::sync::RwLock;


#[cfg(not(feature = "par"))]
#[macro_export]
macro_rules! ascent_m_par {
    ($($tt: tt)*) => {
        ascent::ascent!{ $($tt)* }
    };   
}

#[cfg(feature = "par")]
#[macro_export]
macro_rules! ascent_m_par {
    ($($tt: tt)*) => {
        ascent::ascent_par!{ $($tt)* }
    };   
}

#[cfg(not(feature = "par"))]
#[macro_export]
macro_rules! ascent_run_m_par {
    ($($tt: tt)*) => {
        ascent::ascent_run!{ $($tt)* }
    };   
}

#[cfg(feature = "par")]
#[macro_export]
macro_rules! ascent_run_m_par {
    ($($tt: tt)*) => {
        ascent::ascent_run_par!{ $($tt)* }
    };   
}

#[cfg(not(feature = "par"))]
#[allow(dead_code)]
pub fn lat_to_vec<T>(vec: Vec<T>) -> Vec<T> {
    vec
}

#[cfg(feature = "par")]
#[allow(dead_code)]
pub fn lat_to_vec<T>(vec: ascent::boxcar::Vec<RwLock<T>>) -> Vec<T> {
    vec.into_iter().map(|x| x.into_inner().unwrap()).collect()
}