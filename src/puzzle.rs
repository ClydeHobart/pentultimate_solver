use {
	crate::{
		prelude::*,
		get_data,
		colors::{
			ColorDataWithMat,
			ColorData,
			MatHdl
		},
		math::polyhedra::{
			data::{
				Data,
				FaceData
			},
			Polyhedron
		},
		piece::{
			PieceLibrary,
			PiecePair
		}
	},
	bevy::prelude::*
};

pub fn build_app(app_builder: &mut AppBuilder) {
	app_builder.add_startup_system(startup_app
		.system()
		.label("puzzle::startup_app()")
		.after("PieceLibrary::startup_app()")
		.after("ColorDataTyped::startup_app()")
	);
}

fn startup_app(
	mut commands: Commands,
	piece_library: Res<PieceLibrary>,
	color_data: Res<ColorData<Color>>
) -> () {
	log_error_result!(startup_app_internal(&mut commands, &piece_library, &color_data));
}

fn startup_app_internal(
	commands: &mut Commands,
	piece_library: &Res<PieceLibrary>,
	color_data: &Res<ColorData<Color>>
) -> LogErrorResult {
	let piece_pair: &PiecePair = match piece_library.pieces.get(&piece_library.data.default_design) {
		Some(piece_pair) => piece_pair,
		None => {
			return Err(log_error!(
				Level::Error,
				format!("No PiecePair available for Design {:?}",
					piece_library.data.default_design
				)
			))
		}
	};
	let (base_mat, color_mats): (&MatHdl, &Vec<MatHdl>) = {
		let mats: &ColorDataWithMat<MatHdl> = match color_data.mats.as_ref() {
			Some(mats) => mats,
			None => {
				return Err(log_error!(
					Level::Error,
					format!("ColorData had None for mats")
				));
			}
		};

		(
			&mats.base_color,
			match mats.polyhedron_to_colors.get(&piece_library.data.default_design.as_polyhedron()) {
				Some(color_mats) => color_mats,
				None => {
					return Err(log_error!(
						Level::Error,
						format!("No color materials available for Design {:?}'s Polyhedron, {:?}",
							piece_library.data.default_design,
							piece_library.data.default_design.as_polyhedron()
						)
					));
				}
			}
		)
	};
	let faces: &Vec<FaceData> = &get_data!(Icosidodecahedron).faces;
	let param_bundle: (&MatHdl, &Vec<MatHdl>, &Vec<FaceData>) = (base_mat, color_mats, faces);

	piece_pair.add_entities(commands, &param_bundle);

	Ok(())
}