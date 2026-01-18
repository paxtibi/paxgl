
const
  STD_VIDEO_AV1_NUM_REF_FRAMES = 8;
  STD_VIDEO_AV1_REFS_PER_FRAME = 7;
  STD_VIDEO_AV1_TOTAL_REFS_PER_FRAME = 8;
  STD_VIDEO_AV1_MAX_TILE_COLS = 64;
  STD_VIDEO_AV1_MAX_TILE_ROWS = 64;
  STD_VIDEO_AV1_MAX_SEGMENTS = 8;
  STD_VIDEO_AV1_SEG_LVL_MAX = 8;
  STD_VIDEO_AV1_PRIMARY_REF_NONE = 7;
  STD_VIDEO_AV1_SELECT_INTEGER_MV = 2;
  STD_VIDEO_AV1_SELECT_SCREEN_CONTENT_TOOLS = 2;
  STD_VIDEO_AV1_SKIP_MODE_FRAMES = 2;
  STD_VIDEO_AV1_MAX_LOOP_FILTER_STRENGTHS = 4;
  STD_VIDEO_AV1_LOOP_FILTER_ADJUSTMENTS = 2;
  STD_VIDEO_AV1_MAX_CDEF_FILTER_STRENGTHS = 8;
  STD_VIDEO_AV1_MAX_NUM_PLANES = 3;
  STD_VIDEO_AV1_GLOBAL_MOTION_PARAMS = 6;
  STD_VIDEO_AV1_MAX_NUM_Y_POINTS = 14;
  STD_VIDEO_AV1_MAX_NUM_CB_POINTS = 10;
  STD_VIDEO_AV1_MAX_NUM_CR_POINTS = 10;
  STD_VIDEO_AV1_MAX_NUM_POS_LUMA = 24;
  STD_VIDEO_AV1_MAX_NUM_POS_CHROMA = 25;


type
  StdVideoAV1TileInfo = record
  end;   // forward
  StdVideoAV1Quantization = record
  end;
  StdVideoAV1Segmentation = record
  end;
  StdVideoAV1LoopFilter = record
  end;
  StdVideoAV1CDEF = record
  end;
  StdVideoAV1LoopRestoration = record
  end;
  StdVideoAV1GlobalMotion = record
  end;
  StdVideoAV1FilmGrain = record
  end;

  PStdVideoAV1TileInfo = ^StdVideoAV1TileInfo;
  PStdVideoAV1Quantization = ^StdVideoAV1Quantization;
  PStdVideoAV1Segmentation = ^StdVideoAV1Segmentation;
  PStdVideoAV1LoopFilter = ^StdVideoAV1LoopFilter;
  PStdVideoAV1CDEF = ^StdVideoAV1CDEF;
  PStdVideoAV1LoopRestoration = ^StdVideoAV1LoopRestoration;
  PStdVideoAV1GlobalMotion = ^StdVideoAV1GlobalMotion;
  PStdVideoAV1FilmGrain = ^StdVideoAV1FilmGrain;

  StdVideoAV1Profile = (
    STD_VIDEO_AV1_PROFILE_MAIN = 0,
    STD_VIDEO_AV1_PROFILE_HIGH = 1,
    STD_VIDEO_AV1_PROFILE_PROFESSIONAL = 2,
    STD_VIDEO_AV1_PROFILE_INVALID = $7FFFFFFF,
    STD_VIDEO_AV1_PROFILE_MAX_ENUM = $7FFFFFFF
    );

  StdVideoAV1Level = (
    STD_VIDEO_AV1_LEVEL_2_0 = 0,
    STD_VIDEO_AV1_LEVEL_2_1 = 1,
    STD_VIDEO_AV1_LEVEL_2_2 = 2,
    STD_VIDEO_AV1_LEVEL_2_3 = 3,
    STD_VIDEO_AV1_LEVEL_3_0 = 4,
    STD_VIDEO_AV1_LEVEL_3_1 = 5,
    STD_VIDEO_AV1_LEVEL_3_2 = 6,
    STD_VIDEO_AV1_LEVEL_3_3 = 7,
    STD_VIDEO_AV1_LEVEL_4_0 = 8,
    STD_VIDEO_AV1_LEVEL_4_1 = 9,
    STD_VIDEO_AV1_LEVEL_4_2 = 10,
    STD_VIDEO_AV1_LEVEL_4_3 = 11,
    STD_VIDEO_AV1_LEVEL_5_0 = 12,
    STD_VIDEO_AV1_LEVEL_5_1 = 13,
    STD_VIDEO_AV1_LEVEL_5_2 = 14,
    STD_VIDEO_AV1_LEVEL_5_3 = 15,
    STD_VIDEO_AV1_LEVEL_6_0 = 16,
    STD_VIDEO_AV1_LEVEL_6_1 = 17,
    STD_VIDEO_AV1_LEVEL_6_2 = 18,
    STD_VIDEO_AV1_LEVEL_6_3 = 19,
    STD_VIDEO_AV1_LEVEL_7_0 = 20,
    STD_VIDEO_AV1_LEVEL_7_1 = 21,
    STD_VIDEO_AV1_LEVEL_7_2 = 22,
    STD_VIDEO_AV1_LEVEL_7_3 = 23,
    STD_VIDEO_AV1_LEVEL_INVALID = $7FFFFFFF,
    STD_VIDEO_AV1_LEVEL_MAX_ENUM = $7FFFFFFF
    );

  StdVideoAV1FrameType = (
    STD_VIDEO_AV1_FRAME_TYPE_KEY = 0,
    STD_VIDEO_AV1_FRAME_TYPE_INTER = 1,
    STD_VIDEO_AV1_FRAME_TYPE_INTRA_ONLY = 2,
    STD_VIDEO_AV1_FRAME_TYPE_SWITCH = 3,
    STD_VIDEO_AV1_FRAME_TYPE_INVALID = $7FFFFFFF,
    STD_VIDEO_AV1_FRAME_TYPE_MAX_ENUM = $7FFFFFFF
    );

  StdVideoAV1ReferenceName = (
    STD_VIDEO_AV1_REFERENCE_NAME_INTRA_FRAME = 0,
    STD_VIDEO_AV1_REFERENCE_NAME_LAST_FRAME = 1,
    STD_VIDEO_AV1_REFERENCE_NAME_LAST2_FRAME = 2,
    STD_VIDEO_AV1_REFERENCE_NAME_LAST3_FRAME = 3,
    STD_VIDEO_AV1_REFERENCE_NAME_GOLDEN_FRAME = 4,
    STD_VIDEO_AV1_REFERENCE_NAME_BWDREF_FRAME = 5,
    STD_VIDEO_AV1_REFERENCE_NAME_ALTREF2_FRAME = 6,
    STD_VIDEO_AV1_REFERENCE_NAME_ALTREF_FRAME = 7,
    STD_VIDEO_AV1_REFERENCE_NAME_INVALID = $7FFFFFFF,
    STD_VIDEO_AV1_REFERENCE_NAME_MAX_ENUM = $7FFFFFFF
    );

  StdVideoAV1InterpolationFilter = (
    STD_VIDEO_AV1_INTERPOLATION_FILTER_EIGHTTAP = 0,
    STD_VIDEO_AV1_INTERPOLATION_FILTER_EIGHTTAP_SMOOTH = 1,
    STD_VIDEO_AV1_INTERPOLATION_FILTER_EIGHTTAP_SHARP = 2,
    STD_VIDEO_AV1_INTERPOLATION_FILTER_BILINEAR = 3,
    STD_VIDEO_AV1_INTERPOLATION_FILTER_SWITCHABLE = 4,
    STD_VIDEO_AV1_INTERPOLATION_FILTER_INVALID = $7FFFFFFF,
    STD_VIDEO_AV1_INTERPOLATION_FILTER_MAX_ENUM = $7FFFFFFF
    );

  StdVideoAV1TxMode = (
    STD_VIDEO_AV1_TX_MODE_ONLY_4X4 = 0,
    STD_VIDEO_AV1_TX_MODE_LARGEST = 1,
    STD_VIDEO_AV1_TX_MODE_SELECT = 2,
    STD_VIDEO_AV1_TX_MODE_INVALID = $7FFFFFFF,
    STD_VIDEO_AV1_TX_MODE_MAX_ENUM = $7FFFFFFF
    );

  StdVideoAV1FrameRestorationType = (
    STD_VIDEO_AV1_FRAME_RESTORATION_TYPE_NONE = 0,
    STD_VIDEO_AV1_FRAME_RESTORATION_TYPE_WIENER = 1,
    STD_VIDEO_AV1_FRAME_RESTORATION_TYPE_SGRPROJ = 2,
    STD_VIDEO_AV1_FRAME_RESTORATION_TYPE_SWITCHABLE = 3,
    STD_VIDEO_AV1_FRAME_RESTORATION_TYPE_INVALID = $7FFFFFFF,
    STD_VIDEO_AV1_FRAME_RESTORATION_TYPE_MAX_ENUM = $7FFFFFFF
    );

  StdVideoAV1ColorPrimaries = (
    STD_VIDEO_AV1_COLOR_PRIMARIES_BT_709 = 1,
    STD_VIDEO_AV1_COLOR_PRIMARIES_UNSPECIFIED = 2,
    STD_VIDEO_AV1_COLOR_PRIMARIES_BT_470_M = 4,
    STD_VIDEO_AV1_COLOR_PRIMARIES_BT_470_B_G = 5,
    STD_VIDEO_AV1_COLOR_PRIMARIES_BT_601 = 6,
    STD_VIDEO_AV1_COLOR_PRIMARIES_SMPTE_240 = 7,
    STD_VIDEO_AV1_COLOR_PRIMARIES_GENERIC_FILM = 8,
    STD_VIDEO_AV1_COLOR_PRIMARIES_BT_2020 = 9,
    STD_VIDEO_AV1_COLOR_PRIMARIES_XYZ = 10,
    STD_VIDEO_AV1_COLOR_PRIMARIES_SMPTE_431 = 11,
    STD_VIDEO_AV1_COLOR_PRIMARIES_SMPTE_432 = 12,
    STD_VIDEO_AV1_COLOR_PRIMARIES_EBU_3213 = 22,
    STD_VIDEO_AV1_COLOR_PRIMARIES_INVALID = $7FFFFFFF,
    STD_VIDEO_AV1_COLOR_PRIMARIES_BT_UNSPECIFIED = 2,  // alias
    STD_VIDEO_AV1_COLOR_PRIMARIES_MAX_ENUM = $7FFFFFFF
    );

  StdVideoAV1TransferCharacteristics = (
    STD_VIDEO_AV1_TRANSFER_CHARACTERISTICS_RESERVED_0 = 0,
    STD_VIDEO_AV1_TRANSFER_CHARACTERISTICS_BT_709 = 1,
    STD_VIDEO_AV1_TRANSFER_CHARACTERISTICS_UNSPECIFIED = 2,
    STD_VIDEO_AV1_TRANSFER_CHARACTERISTICS_RESERVED_3 = 3,
    STD_VIDEO_AV1_TRANSFER_CHARACTERISTICS_BT_470_M = 4,
    STD_VIDEO_AV1_TRANSFER_CHARACTERISTICS_BT_470_B_G = 5,
    STD_VIDEO_AV1_TRANSFER_CHARACTERISTICS_BT_601 = 6,
    STD_VIDEO_AV1_TRANSFER_CHARACTERISTICS_SMPTE_240 = 7,
    STD_VIDEO_AV1_TRANSFER_CHARACTERISTICS_LINEAR = 8,
    STD_VIDEO_AV1_TRANSFER_CHARACTERISTICS_LOG_100 = 9,
    STD_VIDEO_AV1_TRANSFER_CHARACTERISTICS_LOG_100_SQRT10 = 10,
    STD_VIDEO_AV1_TRANSFER_CHARACTERISTICS_IEC_61966 = 11,
    STD_VIDEO_AV1_TRANSFER_CHARACTERISTICS_BT_1361 = 12,
    STD_VIDEO_AV1_TRANSFER_CHARACTERISTICS_SRGB = 13,
    STD_VIDEO_AV1_TRANSFER_CHARACTERISTICS_BT_2020_10_BIT = 14,
    STD_VIDEO_AV1_TRANSFER_CHARACTERISTICS_BT_2020_12_BIT = 15,
    STD_VIDEO_AV1_TRANSFER_CHARACTERISTICS_SMPTE_2084 = 16,
    STD_VIDEO_AV1_TRANSFER_CHARACTERISTICS_SMPTE_428 = 17,
    STD_VIDEO_AV1_TRANSFER_CHARACTERISTICS_HLG = 18,
    STD_VIDEO_AV1_TRANSFER_CHARACTERISTICS_INVALID = $7FFFFFFF,
    STD_VIDEO_AV1_TRANSFER_CHARACTERISTICS_MAX_ENUM = $7FFFFFFF
    );

  StdVideoAV1MatrixCoefficients = (
    STD_VIDEO_AV1_MATRIX_COEFFICIENTS_IDENTITY = 0,
    STD_VIDEO_AV1_MATRIX_COEFFICIENTS_BT_709 = 1,
    STD_VIDEO_AV1_MATRIX_COEFFICIENTS_UNSPECIFIED = 2,
    STD_VIDEO_AV1_MATRIX_COEFFICIENTS_RESERVED_3 = 3,
    STD_VIDEO_AV1_MATRIX_COEFFICIENTS_FCC = 4,
    STD_VIDEO_AV1_MATRIX_COEFFICIENTS_BT_470_B_G = 5,
    STD_VIDEO_AV1_MATRIX_COEFFICIENTS_BT_601 = 6,
    STD_VIDEO_AV1_MATRIX_COEFFICIENTS_SMPTE_240 = 7,
    STD_VIDEO_AV1_MATRIX_COEFFICIENTS_SMPTE_YCGCO = 8,
    STD_VIDEO_AV1_MATRIX_COEFFICIENTS_BT_2020_NCL = 9,
    STD_VIDEO_AV1_MATRIX_COEFFICIENTS_BT_2020_CL = 10,
    STD_VIDEO_AV1_MATRIX_COEFFICIENTS_SMPTE_2085 = 11,
    STD_VIDEO_AV1_MATRIX_COEFFICIENTS_CHROMAT_NCL = 12,
    STD_VIDEO_AV1_MATRIX_COEFFICIENTS_CHROMAT_CL = 13,
    STD_VIDEO_AV1_MATRIX_COEFFICIENTS_ICTCP = 14,
    STD_VIDEO_AV1_MATRIX_COEFFICIENTS_INVALID = $7FFFFFFF,
    STD_VIDEO_AV1_MATRIX_COEFFICIENTS_MAX_ENUM = $7FFFFFFF
    );

  StdVideoAV1ChromaSamplePosition = (
    STD_VIDEO_AV1_CHROMA_SAMPLE_POSITION_UNKNOWN = 0,
    STD_VIDEO_AV1_CHROMA_SAMPLE_POSITION_VERTICAL = 1,
    STD_VIDEO_AV1_CHROMA_SAMPLE_POSITION_COLOCATED = 2,
    STD_VIDEO_AV1_CHROMA_SAMPLE_POSITION_RESERVED = 3,
    STD_VIDEO_AV1_CHROMA_SAMPLE_POSITION_INVALID = $7FFFFFFF,
    STD_VIDEO_AV1_CHROMA_SAMPLE_POSITION_MAX_ENUM = $7FFFFFFF
    );

  StdVideoAV1ColorConfigFlags = bitpacked record
    mono_chrome: 0..1;
    color_range: 0..1;
    separate_uv_delta_q: 0..1;
    color_description_present_flag: 0..1;
    reserved: 0..(1 shl 28) - 1;
  end;

  StdVideoAV1ColorConfig = record
    flags: StdVideoAV1ColorConfigFlags;
    BitDepth: uint8;
    subsampling_x: uint8;
    subsampling_y: uint8;
    reserved1: uint8;
    color_primaries: StdVideoAV1ColorPrimaries;
    transfer_characteristics: StdVideoAV1TransferCharacteristics;
    matrix_coefficients: StdVideoAV1MatrixCoefficients;
    chroma_sample_position: StdVideoAV1ChromaSamplePosition;
  end;

  // ---------------------------------------------------------------------------
  //  Tipi decode specifici
  // ---------------------------------------------------------------------------

  StdVideoDecodeAV1PictureInfoFlags = bitpacked record
    error_resilient_mode: 0..1;
    disable_cdf_update: 0..1;
    use_superres: 0..1;
    render_and_frame_size_different: 0..1;
    allow_screen_content_tools: 0..1;
    is_filter_switchable: 0..1;
    force_integer_mv: 0..1;
    frame_size_override_flag: 0..1;
    buffer_removal_time_present_flag: 0..1;
    allow_intrabc: 0..1;
    frame_refs_short_signaling: 0..1;
    allow_high_precision_mv: 0..1;
    is_motion_mode_switchable: 0..1;
    use_ref_frame_mvs: 0..1;
    disable_frame_end_update_cdf: 0..1;
    allow_warped_motion: 0..1;
    reduced_tx_set: 0..1;
    reference_select: 0..1;
    skip_mode_present: 0..1;
    delta_q_present: 0..1;
    delta_lf_present: 0..1;
    delta_lf_multi: 0..1;
    segmentation_enabled: 0..1;
    segmentation_update_map: 0..1;
    segmentation_temporal_update: 0..1;
    segmentation_update_data: 0..1;
    UsesLr: 0..1;
    usesChromaLr: 0..1;
    apply_grain: 0..1;
    reserved: 0..7;
  end;

  PStdVideoDecodeAV1PictureInfo = ^StdVideoDecodeAV1PictureInfo;

  StdVideoDecodeAV1PictureInfo = record
    flags: StdVideoDecodeAV1PictureInfoFlags;
    frame_type: StdVideoAV1FrameType;
    current_frame_id: uint32;
    OrderHint: uint8;
    primary_ref_frame: uint8;
    refresh_frame_flags: uint8;
    reserved1: uint8;
    interpolation_filter: StdVideoAV1InterpolationFilter;
    TxMode: StdVideoAV1TxMode;
    delta_q_res: uint8;
    delta_lf_res: uint8;
    SkipModeFrame: array[0..STD_VIDEO_AV1_SKIP_MODE_FRAMES - 1] of uint8;
    coded_denom: uint8;
    reserved2: array[0..2] of uint8;
    OrderHints: array[0..STD_VIDEO_AV1_NUM_REF_FRAMES - 1] of uint8;
    expectedFrameId: array[0..STD_VIDEO_AV1_NUM_REF_FRAMES - 1] of uint32;
    pTileInfo: ^StdVideoAV1TileInfo;
    pQuantization: ^StdVideoAV1Quantization;
    pSegmentation: ^StdVideoAV1Segmentation;
    pLoopFilter: ^StdVideoAV1LoopFilter;
    pCDEF: ^StdVideoAV1CDEF;
    pLoopRestoration: ^StdVideoAV1LoopRestoration;
    pGlobalMotion: ^StdVideoAV1GlobalMotion;
    pFilmGrain: ^StdVideoAV1FilmGrain;
  end;

  StdVideoDecodeAV1ReferenceInfoFlags = bitpacked record
    disable_frame_end_update_cdf: 0..1;
    segmentation_enabled: 0..1;
    reserved: 0..(1 shl 30) - 1;
  end;

  PStdVideoDecodeAV1ReferenceInfo = ^StdVideoDecodeAV1ReferenceInfo;

  StdVideoDecodeAV1ReferenceInfo = record
    flags: StdVideoDecodeAV1ReferenceInfoFlags;
    frame_type: uint8;
    RefFrameSignBias: uint8;
    OrderHint: uint8;
    SavedOrderHints: array[0..STD_VIDEO_AV1_NUM_REF_FRAMES - 1] of uint8;
  end;

  // ---------------------------------------------------------------------------
  //  Costanti estensioni Vulkan (decode & encode)
  // ---------------------------------------------------------------------------

const
  VK_STD_VULKAN_VIDEO_CODEC_AV1_DECODE_EXTENSION_NAME = 'VK_STD_vulkan_video_codec_av1_decode';
  VK_STD_VULKAN_VIDEO_CODEC_AV1_ENCODE_EXTENSION_NAME = 'VK_STD_vulkan_video_codec_av1_encode';

