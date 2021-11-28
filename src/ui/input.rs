use {
	crate::{
		prelude::*,
		app::prelude::*,
		math::polyhedra::{
			data::{
				Data,
				FaceData
			},
			Polyhedron
		},
		puzzle::{
			consts::*,
			transformation::{
				Action as TransformationAction,
				FullAddr,
				GetWord,
				HalfAddr,
				Type as TransformationType
			},
			ExtendedPuzzleState,
			InflatedPuzzleState
		}
	},
	super::{
		Preferences,
		View
	},
	std::time::{
		Duration,
		Instant
	},
	bevy::{
		prelude::*,
		app::CoreStage,
		input::{
			keyboard::KeyCode as BevyKeyCode,
			mouse::{
				MouseMotion,
				MouseWheel
			}
		}
	},
	bevy_inspector_egui::{
		egui,
		Context,
		Inspectable
	},
	serde::Deserialize
};

pub fn generate_default_positions() -> [usize; HALF_PENTAGON_PIECE_COUNT] {
	let mut positions: [usize; HALF_PENTAGON_PIECE_COUNT] = [0_usize; HALF_PENTAGON_PIECE_COUNT];
	let icosidodecahedron_data: &Data = match Data::get(Polyhedron::Icosidodecahedron) {
		Some(icosidodecahedron_data) => icosidodecahedron_data,
		None => {
			return positions;
		}
	};
	let face_0_data: &FaceData = &icosidodecahedron_data.faces[0_usize];
	let face_1_norm: Vec3 = icosidodecahedron_data.faces[1_usize].norm;

	for rotation in 3_u32 ..= 7_u32 {
		positions[rotation as usize - 2] = icosidodecahedron_data.get_closest_face_index(&face_0_data.get_rotation_quat(rotation % PENTAGON_SIDE_COUNT as u32).mul_vec3(face_1_norm), None);
	}

	positions
}

#[derive(Clone, Copy, Deserialize, PartialEq)]
#[serde(from = "bevy::input::keyboard::KeyCode")]
pub struct KeyCode(BevyKeyCode);

impl From<BevyKeyCode> for KeyCode { fn from(bevy_key_code: BevyKeyCode) -> Self { Self(bevy_key_code) } }
impl From<KeyCode> for BevyKeyCode { fn from(key_code: KeyCode) -> Self { key_code.0 } }

impl Inspectable for KeyCode {
	type Attributes = ();

	fn ui(&mut self, ui: &mut egui::Ui, _: Self::Attributes, context: &Context) -> bool {
		let mut changed: bool = false;

		egui::ComboBox::from_id_source(context.id())
			.selected_text(format!("{:?}", self.0))
			.show_ui(ui, |ui: &mut egui::Ui| -> () {
				macro_rules! ui_for_key_codes {
					($($variant:ident),*) => {
						$(
							if ui.selectable_label(
									matches!(self.0, BevyKeyCode::$variant),
									format!("{:?}", BevyKeyCode::$variant)
							).clicked() {
								self.0 = BevyKeyCode::$variant;
								changed = true;
							}
						)*
					}
				}

				ui_for_key_codes!(
					Key1, Key2, Key3, Key4, Key5, Key6, Key7, Key8, Key9, Key0,
					A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
					Escape,
					F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12,
					F13, F14, F15, F16, F17, F18, F19, F20, F21, F22, F23, F24,
					Snapshot,
					Scroll,
					Pause,
					Insert,
					Home,
					Delete,
					End,
					PageDown, PageUp,
					Left, Up, Right, Down,
					Back,
					Return,
					Space,
					Compose,
					Caret,
					Numlock, Numpad0, Numpad1, Numpad2, Numpad3, Numpad4, Numpad5, Numpad6, Numpad7, Numpad8, Numpad9,
					NumpadAdd,
					NumpadSubtract,
					NumpadMultiply,
					NumpadDivide,
					NumpadDecimal,
					NumpadComma,
					NumpadEnter,
					NumpadEquals,
					AbntC1, AbntC2,
					Apostrophe,
					Apps,
					Asterisk,
					Plus,
					At,
					Ax,
					Backslash,
					Calculator,
					Capital,
					Colon,
					Comma,
					Convert,
					Equals,
					Grave,
					Kana,
					Kanji,
					LAlt,
					LBracket,
					LControl,
					LShift,
					LWin,
					Mail,
					MediaSelect,
					MediaStop,
					Minus,
					Mute,
					MyComputer,
					NavigateForward,
					NavigateBackward,
					NextTrack,
					NoConvert,
					Oem102,
					Period,
					PlayPause,
					Power,
					PrevTrack,
					RAlt,
					RBracket,
					RControl,
					RShift,
					RWin,
					Semicolon,
					Slash,
					Sleep,
					Stop,
					Sysrq,
					Tab,
					Underline,
					Unlabeled,
					VolumeDown, VolumeUp,
					Wake,
					WebBack, WebFavorites, WebForward, WebHome, WebRefresh, WebSearch, WebStop,
					Yen,
					Copy, Paste, Cut
				);
			});

		changed
	}
}

macro_rules! bkc {
	($variant:ident) => {
		KeyCode::from(BevyKeyCode::$variant)
	}
}


define_struct_with_default!(
	#[derive(Clone, Deserialize, Inspectable, PartialEq)]
	pub InputData {
		#[inspectable(collapse)]
		pub default_positions:		[usize; HALF_PENTAGON_PIECE_COUNT]		= generate_default_positions(),
		#[inspectable(collapse)]
		pub rotation_keys:			[KeyCode; HALF_PENTAGON_PIECE_COUNT]	= [bkc!(Numpad0), bkc!(Numpad1), bkc!(Numpad4), bkc!(Numpad5), bkc!(Numpad6), bkc!(Numpad3)],
		pub recenter_camera:		KeyCode									= bkc!(Space),
		pub undo:					KeyCode									= bkc!(Left),
		pub redo:					KeyCode									= bkc!(Right),
		pub rotate_twice:			KeyCode									= bkc!(D),
		pub counter_clockwise:		KeyCode									= bkc!(S),
		pub alt_hemi:				KeyCode									= bkc!(A),
		pub disable_recentering:	KeyCode									= bkc!(X),
	}
);

pub struct ActiveTransformationAction {
	pub action:				TransformationAction,
	start:					Instant,
	duration:				Duration,
	pub camera_orientation:	Option<Quat>
}

impl ActiveTransformationAction {
	pub fn s_now(&self) -> Option<f32> {
		if self.duration.is_zero() {
			None
		} else {
			let s: f32 = (Instant::now() - self.start).as_millis() as f32 / self.duration.as_millis() as f32;
	
			if s < 1.0_f32 {
				Some(s)
			} else {
				None
			}
		}
	}
}

#[derive(Clone, Copy, Default)]
pub struct InputToggles {
	pub rotate_twice:					bool,
	pub counter_clockwise:				bool,
	pub alt_hemi:						bool,
	pub disable_recentering:			bool
}

pub enum Action {
	None,
	Transformation(ActiveTransformationAction),
	Undo(TransformationAction),
	Redo(TransformationAction)
}

impl Default for Action { fn default() -> Self { Self::None } }

#[derive(Default)]
pub struct InputState {
	pub toggles:						InputToggles,
	pub action:							Action,
	pub camera_rotation:				Quat,
}

pub struct InputPlugin;

impl InputPlugin {
	fn run(
		extended_puzzle_state:		Res<ExtendedPuzzleState>,
		keyboard_input:				Res<Input<BevyKeyCode>>,
		mouse_button_input:			Res<Input<MouseButton>>,
		view:						Res<View>,
		time:						Res<Time>,
		preferences:				Res<Preferences>,
		polyhedra_data_library:		Res<PolyhedraDataLibrary>,
		transformation_library:		Res<TransformationLibraryRef>,
		mut mouse_motion_events:	EventReader<MouseMotion>,
		mut mouse_wheel_events:		EventReader<MouseWheel>,
		mut input_state:			ResMut<InputState>,
		mut camera_component_query:	Query<(&mut CameraComponent, &Transform)>
	) -> () {
		if !matches!(*view, View::Main) {
			input_state.camera_rotation = Quat::IDENTITY;

			return;
		}

		let input_data: &InputData = &preferences.input;
		let mut toggles: InputToggles = input_state.toggles;

		macro_rules! check_toggle {
			($key_code:ident, $toggle:ident) => {
				if keyboard_input.just_pressed(input_data.$key_code.into()) { toggles.$toggle = !toggles.$toggle; }
			}
		}

		check_toggle!(rotate_twice,			rotate_twice);
		check_toggle!(counter_clockwise,	counter_clockwise);
		check_toggle!(alt_hemi,				alt_hemi);
		check_toggle!(disable_recentering,	disable_recentering);

		let mut rolled_or_panned: bool = false;

		let (mouse_motion_delta, mouse_wheel_delta): (Vec2, f32) = {
			let time_delta: f32 = time.delta_seconds();

			(
				if mouse_button_input.pressed(MouseButton::Middle) {
					mouse_motion_events
						.iter()
						.map(|mouse_motion: &MouseMotion| -> &Vec2 {
							&mouse_motion.delta
						})
						.sum::<Vec2>() / time_delta
				} else {
					Vec2::ZERO
				},
				mouse_wheel_events
					.iter()
					.map(|mouse_wheel: &MouseWheel| -> f32 {
						mouse_wheel.y
					})
					.sum::<f32>() / time_delta
			)
		};

		input_state.camera_rotation = {
			if !mouse_motion_delta.abs_diff_eq(Vec2::ZERO, f32::EPSILON) {
				const PAN_SCALING_FACTOR: f32 = 500_000.0_f32;

				rolled_or_panned = true;

				Quat::from_axis_angle(
					Vec3::new(mouse_motion_delta.x, -mouse_motion_delta.y, 0.0_f32).cross(Vec3::Z).normalize(),
					mouse_motion_delta.length() / PAN_SCALING_FACTOR * preferences.speed.pan_speed as f32
				)
			} else if mouse_wheel_delta.abs() > f32::EPSILON {
				const ROLL_SCALING_FACTOR: f32 = 5_000.0_f32;

				rolled_or_panned = true;

				Quat::from_rotation_z(
					mouse_wheel_delta / ROLL_SCALING_FACTOR * preferences.speed.roll_speed as f32
				)
			} else {
				Quat::IDENTITY
			}
		};

		match &mut input_state.action {
			Action::None => {
				let mut default_position: Option<usize> = None;

				for (index, key_code) in input_data.rotation_keys.iter().enumerate() {
					if keyboard_input.just_pressed((*key_code).into()) {
						default_position = Some(input_data.default_positions[index]);

						break;
					}
				}

				let camera_orientation: &Quat = &log_option_none!(camera_component_query.iter_mut().next()).1.rotation;
				let camera_start: HalfAddr = CameraPlugin::compute_camera_addr(
					&polyhedra_data_library.icosidodecahedron,
					camera_orientation
				);
				let start: Instant = Instant::now();
				let duration: Duration = Duration::from_millis(preferences.speed.rotation_millis as u64);

				match default_position {
					Some(default_position) => {
						let transformation: FullAddr = (
							TransformationType::StandardRotation as usize,
							transformation_library
								.book_pack_data
								.trfm
								.get_word(
									*transformation_library
										.book_pack_data
										.addr
										.get_word(FullAddr::from((
											TransformationType::Reorientation,
											camera_start
										)))
								)
								.as_ref()
								.pos
								[
									if input_state.toggles.alt_hemi {
										InflatedPuzzleState::invert_position(default_position)
									} else {
										default_position
									}
								]
								as usize,
							(
								1_i32
									* if input_state.toggles.rotate_twice { 2_i32 } else { 1_i32 }
									* if input_state.toggles.counter_clockwise { -1_i32 } else { 1_i32 }
									+ PENTAGON_SIDE_COUNT as i32
							)
								as usize
								% PENTAGON_SIDE_COUNT
						).into();

						input_state.action = Action::Transformation(ActiveTransformationAction {
							action: TransformationAction::new(
								transformation,
								camera_start,
								(
									&extended_puzzle_state.puzzle_state
										+ transformation_library
											.book_pack_data
											.trfm
											.get_word(transformation)
								).standardization_half_addr()
							),
							start,
							duration: if preferences.speed.uniform_transformation_duration {
								duration
							} else {
								duration * transformation.get_cycles()
							},
							camera_orientation: if input_state.toggles.disable_recentering {
								None
							} else {
								Some(*camera_orientation)
							}
						});
					},
					None => {
						if keyboard_input.just_pressed(input_data.recenter_camera.into()) {
							input_state.action = Action::Transformation(ActiveTransformationAction {
								action: TransformationAction::new(
									FullAddr::default(),
									camera_start,
									HalfAddr::default()
								),
								start,
								duration,
								camera_orientation: Some(*camera_orientation)
							});
						} else if keyboard_input.just_pressed(input_data.undo.into())
							&& extended_puzzle_state.curr_action >= 0_i32
						{
							input_state.action = Action::Undo(
								extended_puzzle_state.actions[extended_puzzle_state.curr_action as usize]
							);
						} else if keyboard_input.just_pressed(input_data.redo.into())
							&& ((extended_puzzle_state.curr_action + 1_i32) as usize)
							< extended_puzzle_state.actions.len()
						{
							input_state.action = Action::Redo(
								extended_puzzle_state.actions[(extended_puzzle_state.curr_action + 1_i32)as usize]
							);
						}
					}
				}
			},
			Action::Transformation(active_transformation_action) => {
				if rolled_or_panned {
					// Cancel active camera movement
					active_transformation_action.camera_orientation = None;
				}
			},
			_ => {
				log::warn!("Unexpected input::Action type");
			}
		}

		input_state.toggles = toggles;
	}
}

impl Plugin for InputPlugin {
	fn build(&self, app: &mut AppBuilder) -> () {
		app
			.insert_resource(InputState::default())
			.add_system_to_stage(CoreStage::PreUpdate, Self::run.system());
	}
}