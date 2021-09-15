use {
	crate::{
		prelude::*,
		app::prelude::*
	},
	bevy::prelude::*
};

pub mod camera;
pub mod input;

pub struct UIPlugin;

impl UIPlugin {
	fn startup_app(
		mut windows: ResMut<Windows>
	) -> () {
		log_option_none!(windows.get_primary_mut()).set_maximized(true);
	}
}

impl Plugin for UIPlugin {
	fn build(&self, app: &mut AppBuilder) {
		app
			.insert_resource(from_ron_or_default::<Preferences>(&STRING_DATA.files.preferences))
			.insert_resource(Msaa { samples: 4 })
			.insert_resource(WindowDescriptor {
				title: STRING_DATA.misc.app_title.clone(),
				.. WindowDescriptor::default()
			})
			.add_plugin(CameraPlugin)
			.add_startup_system(Self::startup_app.system());
	}
}