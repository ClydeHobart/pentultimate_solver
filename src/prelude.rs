pub use {
	crate::{
		define_struct_with_default,
		init_log,
		log_concat,
		log_dyn_error,
		log_error,
		log_option_none,
		log_path,
		log_result_err,
		strings::STRING_DATA,
		util::prelude::*,
	},
	std::convert::TryFrom,
	::log::Level
};