pub use {
	crate::{
		strings::STRING_DATA,
		util::prelude::*,
		define_struct_with_default,
		option_to_result,
		log_concat,
		log_dyn_error,
		log_error,
		log_option_none,
		log_path,
		log_result_err
	},
	std::convert::TryFrom,
	::log::Level
};