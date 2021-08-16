extern crate pentultimate_solver;

use {
	bevy::{
		prelude::*,
		render::{
			pipeline::PrimitiveTopology,
			wireframe::{
				Wireframe,
				WireframeConfig,
				WireframePlugin
			}
		},
		wgpu::{
			WgpuFeature,
			WgpuFeatures,
			WgpuOptions
		}
	},
	pentultimate_solver::{
		prelude::*,
		app,
		math::polyhedra::{
			Polyhedron,
			data::*
		},
		util
	}
};

fn init() -> () {
	util::init_env_logger();
}

fn validate() -> Result<(), LogError> {
	Data::validate_polyhedra()?;

	Ok(())
}

fn _spawn_polyhedra_meshes(
	mut commands: Commands,
	mut wireframe_config: ResMut<WireframeConfig>,
	mut meshes: ResMut<Assets<Mesh>>,
	mut materials: ResMut<Assets<StandardMaterial>>
) -> () {
	wireframe_config.global = false;

	for polyhedron_as_u8 in Polyhedron::Icosahedron as u8 ..= Polyhedron::RhombicTriacontahedron as u8 {
		let data: &Data = Data::get(Polyhedron::from(polyhedron_as_u8)).unwrap();
		let x: f32 = 5.0 * (polyhedron_as_u8 as f32) - 12.5;

		commands
			.spawn_bundle(PbrBundle {
				mesh: meshes.add(data.as_mesh(PrimitiveTopology::PointList).unwrap()),
				transform: Transform::from_xyz(x, -5.0, 0.0),
				.. Default::default()
			})
			.insert(Wireframe);

		println!("spawning pointlist mesh at {:?}", Vec3::new(x, -5.0, 0.0));

		commands
			.spawn_bundle(PbrBundle {
				mesh: meshes.add(data.as_mesh(PrimitiveTopology::LineList).unwrap()),
				transform: Transform::from_xyz(x, 0.0, 0.0),
				.. Default::default()
			})
			.insert(Wireframe);

		println!("spawning linelist mesh at {:?}", Vec3::new(x, 0.0, 0.0));

		commands.spawn_bundle(PbrBundle {
			mesh: meshes.add(data.as_mesh(PrimitiveTopology::TriangleList).unwrap()),
			material: materials.add(Color::BLUE.into()),
			transform: Transform::from_xyz(x, 5.0, 0.0),
			.. Default::default()
		});

		println!("spawning trianglelist mesh at {:?}", Vec3::new(x, 5.0, 0.0));
	}

	commands
		.spawn_bundle(PbrBundle {
			mesh: meshes.add(Mesh::from(shape::Cube { size: 0.25 })),
			material: materials.add(Color::rgb(0.8, 0.7, 0.6).into()),
			transform: Transform::from_xyz(0.0, 0.0, 0.0),
			..Default::default()
		})
		// This enables wireframe drawing on this entity
		.insert(Wireframe);

	commands.spawn_bundle(LightBundle {
		transform: Transform::from_xyz(10.0, 10.0, 10.0),
		.. Default::default()
	});

	commands.spawn_bundle(PerspectiveCameraBundle {
		transform: Transform::from_xyz(15.0, -15.0, 10.0).looking_at(Vec3::ZERO, Vec3::Z),
		.. Default::default()
	});
}

fn _run_polyhedra_demo() -> () {
	App::build()
		.insert_resource(Msaa { samples: 4 })
		.insert_resource(WgpuOptions {
			features: WgpuFeatures {
				features: vec![WgpuFeature::NonFillPolygonMode]
			},
			.. Default::default()
		})
		.add_plugins(DefaultPlugins)
		.add_plugin(WireframePlugin)
		.add_startup_system(_spawn_polyhedra_meshes.system())
		.run();
}

fn main() -> () {
	init();
	log_result_err!(validate());
	pentultimate_solver::puzzle::main();
	// app::main();
}