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
		preferences::SpeedData,
		puzzle::{
			consts::*,
			transformation::{
				self,
				packs::*,
				Action,
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
	std::{
		cmp::max,
		collections::VecDeque,
		mem::transmute,
		time::{
			Duration,
			Instant
		}
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
	let icosidodecahedron_data: &Data = Data::get(Polyhedron::Icosidodecahedron);
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
		pub default_positions:				[usize; HALF_PENTAGON_PIECE_COUNT]		= generate_default_positions(),
		#[inspectable(collapse)]
		pub rotation_keys:					[KeyCode; HALF_PENTAGON_PIECE_COUNT]	= [bkc!(Numpad0), bkc!(Numpad1), bkc!(Numpad4), bkc!(Numpad5), bkc!(Numpad6), bkc!(Numpad3)],
		pub recenter_camera:				KeyCode									= bkc!(Space),
		pub undo:							KeyCode									= bkc!(Left),
		pub redo:							KeyCode									= bkc!(Right),
		pub cycle_transformation_type_up:	KeyCode									= bkc!(Up),
		pub cycle_transformation_type_down:	KeyCode									= bkc!(Down),
		pub enable_modifiers:				KeyCode									= bkc!(E),
		pub rotate_twice:					KeyCode									= bkc!(D),
		pub counter_clockwise:				KeyCode									= bkc!(S),
		pub alt_hemi:						KeyCode									= bkc!(A),
		pub disable_recentering:			KeyCode									= bkc!(X),
	}
);

#[derive(Debug)]
pub enum ActionType {
	RecenterCamera,
	Transformation,
	Undo,
	Redo
}

pub struct ActiveAction {
	pub action_type:			ActionType,
	pub actions:				VecDeque<Action>,
	pub camera_orientation:		Option<Quat>,
	start:						Instant,
	duration:					Duration
}

impl ActiveAction {
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

	/// # `update()`
	///
	/// ## Return value
	///
	/// `bool` representing whether or not the action has been completed
	pub fn update(
		&self,
		extended_puzzle_state:	&mut ExtendedPuzzleState,
		queries:				&mut QuerySet<(
			Query<(&mut CameraComponent, &mut Transform)>,
			Query<(&PieceComponent, &mut Transform)>
		)>
	) -> bool {
		let action: Action = if let Some(action) = self.actions.front() {
			*action
		} else {
			return true;
		};
		let end_quat: Quat = if warn_expect!(action.is_valid()) {
			TransformationLibrary::get()
				.orientation_data
				.get_word(action.camera_end())
				.quat
		} else {
			Quat::IDENTITY
		};

		match self.s_now() {
			Some(s) => {
				if action.transformation().is_valid() {
					let comprising_simples: &[HalfAddr] = action
						.transformation()
						.get_comprising_simples();
					let total_cycles: f32 = FullAddr::get_cycles_for_comprising_simples(
						&comprising_simples
					) as f32;
					let mut cycle_count: f32 = 0.0_f32;
					let mut puzzle_state: InflatedPuzzleState = extended_puzzle_state.puzzle_state.clone();

					for comprising_simple in comprising_simples {
						let comprising_simple: FullAddr = (
							TransformationType::Simple,
							*comprising_simple
						).into();
						let cycles: f32 = comprising_simple.get_cycles() as f32;
						let s: f32 = s * total_cycles;

						if cycles == 0.0_f32 {
							continue;
						} else if s >= cycle_count + cycles {
							puzzle_state += comprising_simple;
							cycle_count += cycles;
						} else {
							let word_pack: WordPack = TransformationLibrary::get()
								.book_pack_data
								.get_word_pack(comprising_simple);
							let rotation: Quat = Quat::IDENTITY.short_slerp(*word_pack.quat, 
								(s - cycle_count) / cycles
							);
		
							for (piece_component, mut transform) in queries
								.q1_mut()
								.iter_mut()
							{
								let piece_index: usize = piece_component.index;

								transform.rotation = if word_pack
									.mask
									.affects_piece(puzzle_state.pos[piece_index] as usize)
								{
									rotation
								} else {
									Quat::IDENTITY
								}
									* TransformationLibrary::get()
										.orientation_data
										.get_word(puzzle_state.half_addr(piece_index))
										.quat;
							}

							break;
						}
					}
				}

				if let Some(camera_orientation) = self.camera_orientation {
					if let Some((_, mut transform)) = queries.q0_mut().iter_mut().next() {
						if action.camera_start().is_valid() {
							transform.rotation = camera_orientation.short_slerp(end_quat, s);
						}
					}
				}

				false
			},
			None => {
				let mut standardization_word_pack_option: Option<WordPack> = None;

				if action.transformation().is_valid() {
					let transformation_word_pack: WordPack = TransformationLibrary::get()
						.book_pack_data.
						get_word_pack(*action.transformation());
					let standardization_word_pack: WordPack = TransformationLibrary::get()
						.book_pack_data
						.get_word_pack(action.standardization());

					*extended_puzzle_state += transformation_word_pack.trfm;
					*extended_puzzle_state += standardization_word_pack.trfm;
					warn_expect!(extended_puzzle_state.puzzle_state.is_standardized());

					let puzzle_state: &InflatedPuzzleState = &extended_puzzle_state.puzzle_state;

					for (piece_component, mut transform) in queries
						.q1_mut()
						.iter_mut()
					{
						transform.rotation = TransformationLibrary::get()
							.orientation_data
							.get_word(puzzle_state.half_addr(piece_component.index))
							.quat;
					}

					if !action.transformation().is_page_index_reorientation() {
						standardization_word_pack_option = Some(standardization_word_pack);
					}
				}

				if action.camera_start().is_valid() {
					if let Some((_, mut transform)) = queries.q0_mut().iter_mut().next() {
						transform.rotation = standardization_word_pack_option.map_or(
							Quat::IDENTITY,
							|word_pack: WordPack| -> Quat { *word_pack.quat }
						) * if self.camera_orientation.is_some() {
							end_quat
						} else {
							transform.rotation
						};
					}
				}

				true
			}
		}
	}
}

#[derive(Clone, Copy)]
pub struct InputToggles {
	pub enable_modifiers:		bool,
	pub rotate_twice:			bool,
	pub counter_clockwise:		bool,
	pub alt_hemi:				bool,
	pub disable_recentering:	bool,
	pub transformation_type:	TransformationType,
}

impl InputToggles {
	fn half_addr(&self, default_position: usize) -> HalfAddr {
		HalfAddr::new(self.line_index(default_position), self.word_index())
	}

	fn line_index(&self, default_position: usize) -> usize {
		if self.alt_hemi {
			InflatedPuzzleState::invert_position(default_position)
		} else {
			default_position
		}
	}

	fn word_index(&self) -> usize {
		if self.enable_modifiers {
			(
				1_i32
					* if self.rotate_twice { 2_i32 } else { 1_i32 }
					* if self.counter_clockwise { -1_i32 } else { 1_i32 }
					+ PENTAGON_SIDE_COUNT as i32
			)
				as usize
				% PENTAGON_SIDE_COUNT
		} else {
			0_usize
		}
	}
}

impl Default for InputToggles {
	fn default() -> Self {
		Self {
			enable_modifiers:		true,
			rotate_twice:			false,
			counter_clockwise:		false,
			alt_hemi:				false,
			disable_recentering:	false,
			transformation_type:	TransformationType::Simple
		}
	}
}

#[derive(Default)]
pub struct InputState {
	pub toggles:			InputToggles,
	pub action:				Option<ActiveAction>,
	pub camera_rotation:	Quat,
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
		camera_component_query:		Query<(&CameraComponent, &Transform)>,
		mut mouse_motion_events:	EventReader<MouseMotion>,
		mut mouse_wheel_events:		EventReader<MouseWheel>,
		mut input_state:			ResMut<InputState>
	) -> () {
		if !matches!(*view, View::Main) {
			input_state.camera_rotation = Quat::IDENTITY;

			return;
		}

		let input_data: &InputData = &preferences.input;
		let mut toggles: InputToggles = input_state.toggles;

		macro_rules! check_toggle {
			($toggle:ident) => {
				if keyboard_input.just_pressed(input_data.$toggle.into()) { toggles.$toggle = !toggles.$toggle; }
			}
		}

		check_toggle!(enable_modifiers);
		check_toggle!(rotate_twice);
		check_toggle!(counter_clockwise);
		check_toggle!(alt_hemi);
		check_toggle!(disable_recentering);

		if keyboard_input.just_pressed(input_data.cycle_transformation_type_up.into()) {
			let value_to_transmute: u8 = if toggles.transformation_type as u8 == 0_u8 {
				transformation::TYPE_COUNT as u8
			} else {
				toggles.transformation_type as u8
			} - 1_u8;

			toggles.transformation_type = unsafe { transmute::<u8, TransformationType>(value_to_transmute) };
		}

		if keyboard_input.just_pressed(input_data.cycle_transformation_type_down.into()) {
			let value_to_transmute: u8 = if toggles.transformation_type as u8
				== transformation::TYPE_COUNT as u8 - 1_u8
			{
				0_u8
			} else {
				toggles.transformation_type as u8 + 1_u8
			};

			toggles.transformation_type = unsafe { transmute::<u8, TransformationType>(value_to_transmute) };
		}

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
			None => {
				Self::update_action(
					&extended_puzzle_state,
					&keyboard_input,
					&preferences,
					&camera_component_query,
					&mut input_state
				);
			},
			Some(active_action) => {
				if rolled_or_panned {
					if matches!(active_action.action_type, ActionType::RecenterCamera) {
						/* If the whole purpose of the current active transformation action is to recenter the camera,
						which we no longer wish to do, end the active transformation action entirely */
						input_state.action = None;
					} else {
						// Cancel active camera movement
						active_action.camera_orientation = None;
					}
				}
			}
		}

		input_state.toggles = toggles;
	}

	fn update_action(
		extended_puzzle_state:		&ExtendedPuzzleState,
		keyboard_input:				&Input<BevyKeyCode>,
		preferences:				&Preferences,
		camera_component_query:		&Query<(&CameraComponent, &Transform)>,
		input_state:				&mut InputState
	) -> () {
		let input_data:				&InputData			= &preferences.input;
		let speed_data:				&SpeedData			= &preferences.speed;
		let mut action_type:		Option<ActionType>	= None;
		let mut default_position:	Option<usize>		= None;

		for (index, key_code) in input_data.rotation_keys.iter().enumerate() {
			if keyboard_input.just_pressed((*key_code).into()) {
				action_type = Some(ActionType::Transformation);
				default_position = Some(input_data.default_positions[index]);

				break;
			}
		}

		if action_type.is_none() {
			if keyboard_input.just_pressed(input_data.recenter_camera.into()) {
				action_type = Some(ActionType::RecenterCamera);
			} else if keyboard_input.just_pressed(input_data.undo.into())
				&& extended_puzzle_state.curr_action >= 0_i32
			{
				action_type = Some(ActionType::Undo);
			} else if keyboard_input.just_pressed(input_data.redo.into())
				&& ((extended_puzzle_state.curr_action + 1_i32) as usize)
				< extended_puzzle_state.actions.len()
			{
				action_type = Some(ActionType::Redo);
			}
		}

		if let Some(action_type) = action_type {
			let camera_orientation: &Quat = &log_option_none!(camera_component_query.iter().next()).1.rotation;
			let camera_start: HalfAddr = CameraPlugin::compute_camera_addr(camera_orientation);
			let recenter_camera: bool = matches!(action_type, ActionType::RecenterCamera);
			let action: Action = match action_type {
				ActionType::RecenterCamera => Action::new(
					FullAddr::default(),
					camera_start
				),
				ActionType::Transformation => Action::new(
					FullAddr::from((
						input_state.toggles.transformation_type,
						input_state
							.toggles
							.half_addr(default_position.unwrap())
					)) + camera_start,
					camera_start
				),
				ActionType::Undo => extended_puzzle_state
					.actions
					[extended_puzzle_state.curr_action as usize]
					.invert(),
				ActionType::Redo => extended_puzzle_state
					.actions
					[(extended_puzzle_state.curr_action + 1_i32) as usize]
			};

			if !info_expect!(matches!(action_type, ActionType::RecenterCamera)
				|| action.is_valid()
				&& if action.transformation().is_page_index_reorientation() {
					*action.transformation().get_half_addr() != *action.camera_start()
				} else {
					!action.transformation().is_identity_transformation()
				}
			) {
				debug_expr!(action_type, action);

				return;
			}

			let actions: VecDeque<Action> = VecDeque::<Action>::from([action]);
			let start: Instant = Instant::now();
			let duration: Duration = if matches!(action_type, ActionType::Undo | ActionType::Redo)
				&& !speed_data.animate_undo_and_redo
			{
				Duration::ZERO
			} else {
				Duration::from_millis(speed_data.rotation_millis as u64)
					* if speed_data.uniform_transformation_duration || recenter_camera {
						1_u32
					} else {
						max(action.transformation().get_cycles(), 1_u32)
					}
			};
			let camera_orientation: Option<Quat> = if recenter_camera || !input_state.toggles.disable_recentering {
				Some(*camera_orientation)
			} else {
				None
			};

			input_state.action = Some(
				ActiveAction {
					action_type,
					actions,
					start,
					duration,
					camera_orientation
				}
			);
		}
	}
}

impl Plugin for InputPlugin {
	fn build(&self, app: &mut AppBuilder) -> () {
		app
			.insert_resource(InputState::default())
			.add_system_to_stage(CoreStage::PreUpdate, Self::run.system());
	}
}