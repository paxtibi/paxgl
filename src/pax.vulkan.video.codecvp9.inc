const
  STD_VIDEO_VP9_NUM_REF_FRAMES = 8;
  STD_VIDEO_VP9_REFS_PER_FRAME = 3;
  STD_VIDEO_VP9_MAX_REF_FRAMES = 4;
  STD_VIDEO_VP9_LOOP_FILTER_ADJUSTMENTS = 2;
  STD_VIDEO_VP9_MAX_SEGMENTS = 8;
  STD_VIDEO_VP9_SEG_LVL_MAX = 4;
  STD_VIDEO_VP9_MAX_SEGMENTATION_TREE_PROBS = 7;
  STD_VIDEO_VP9_MAX_SEGMENTATION_PRED_PROB = 3;

type
  StdVideoVP9Profile = (
    STD_VIDEO_VP9_PROFILE_0 = 0,
    STD_VIDEO_VP9_PROFILE_1 = 1,
    STD_VIDEO_VP9_PROFILE_2 = 2,
    STD_VIDEO_VP9_PROFILE_3 = 3,
    STD_VIDEO_VP9_PROFILE_INVALID = $7FFFFFFF,
    STD_VIDEO_VP9_PROFILE_MAX_ENUM = $7FFFFFFF
    );

  StdVideoVP9Level = (
    STD_VIDEO_VP9_LEVEL_1_0 = 0,
    STD_VIDEO_VP9_LEVEL_1_1 = 1,
    STD_VIDEO_VP9_LEVEL_2_0 = 2,
    STD_VIDEO_VP9_LEVEL_2_1 = 3,
    STD_VIDEO_VP9_LEVEL_3_0 = 4,
    STD_VIDEO_VP9_LEVEL_3_1 = 5,
    STD_VIDEO_VP9_LEVEL_4_0 = 6,
    STD_VIDEO_VP9_LEVEL_4_1 = 7,
    STD_VIDEO_VP9_LEVEL_5_0 = 8,
    STD_VIDEO_VP9_LEVEL_5_1 = 9,
    STD_VIDEO_VP9_LEVEL_5_2 = 10,
    STD_VIDEO_VP9_LEVEL_6_0 = 11,
    STD_VIDEO_VP9_LEVEL_6_1 = 12,
    STD_VIDEO_VP9_LEVEL_6_2 = 13,
    STD_VIDEO_VP9_LEVEL_INVALID = $7FFFFFFF,
    STD_VIDEO_VP9_LEVEL_MAX_ENUM = $7FFFFFFF
    );

  StdVideoVP9FrameType = (
    STD_VIDEO_VP9_FRAME_TYPE_KEY = 0,
    STD_VIDEO_VP9_FRAME_TYPE_NON_KEY = 1,
    STD_VIDEO_VP9_FRAME_TYPE_INVALID = $7FFFFFFF,
    STD_VIDEO_VP9_FRAME_TYPE_MAX_ENUM = $7FFFFFFF
    );

  StdVideoVP9ReferenceName = (
    STD_VIDEO_VP9_REFERENCE_NAME_INTRA_FRAME = 0,
    STD_VIDEO_VP9_REFERENCE_NAME_LAST_FRAME = 1,
    STD_VIDEO_VP9_REFERENCE_NAME_GOLDEN_FRAME = 2,
    STD_VIDEO_VP9_REFERENCE_NAME_ALTREF_FRAME = 3,
    STD_VIDEO_VP9_REFERENCE_NAME_INVALID = $7FFFFFFF,
    STD_VIDEO_VP9_REFERENCE_NAME_MAX_ENUM = $7FFFFFFF
    );

  StdVideoVP9InterpolationFilter = (
    STD_VIDEO_VP9_INTERPOLATION_FILTER_EIGHTTAP = 0,
    STD_VIDEO_VP9_INTERPOLATION_FILTER_EIGHTTAP_SMOOTH = 1,
    STD_VIDEO_VP9_INTERPOLATION_FILTER_EIGHTTAP_SHARP = 2,
    STD_VIDEO_VP9_INTERPOLATION_FILTER_BILINEAR = 3,
    STD_VIDEO_VP9_INTERPOLATION_FILTER_SWITCHABLE = 4,
    STD_VIDEO_VP9_INTERPOLATION_FILTER_INVALID = $7FFFFFFF,
    STD_VIDEO_VP9_INTERPOLATION_FILTER_MAX_ENUM = $7FFFFFFF
    );

  StdVideoVP9ColorSpace = (
    STD_VIDEO_VP9_COLOR_SPACE_UNKNOWN = 0,
    STD_VIDEO_VP9_COLOR_SPACE_BT_601 = 1,
    STD_VIDEO_VP9_COLOR_SPACE_BT_709 = 2,
    STD_VIDEO_VP9_COLOR_SPACE_SMPTE_170 = 3,
    STD_VIDEO_VP9_COLOR_SPACE_SMPTE_240 = 4,
    STD_VIDEO_VP9_COLOR_SPACE_BT_2020 = 5,
    STD_VIDEO_VP9_COLOR_SPACE_RESERVED = 6,
    STD_VIDEO_VP9_COLOR_SPACE_RGB = 7,
    STD_VIDEO_VP9_COLOR_SPACE_INVALID = $7FFFFFFF,
    STD_VIDEO_VP9_COLOR_SPACE_MAX_ENUM = $7FFFFFFF
    );

  StdVideoVP9ColorConfigFlags = bitpacked record
    color_range: 0..1;
    reserved: 0..31;
  end;

  StdVideoVP9ColorConfig = record
    flags: StdVideoVP9ColorConfigFlags;
    BitDepth: uint8;
    subsampling_x: uint8;
    subsampling_y: uint8;
    reserved1: uint8;
    color_space: StdVideoVP9ColorSpace;
  end;

  StdVideoVP9LoopFilterFlags = bitpacked record
    loop_filter_delta_enabled: 0..1;
    loop_filter_delta_update: 0..1;
    reserved: 0..30;
  end;

  StdVideoVP9LoopFilter = record
    flags: StdVideoVP9LoopFilterFlags;
    loop_filter_level: uint8;
    loop_filter_sharpness: uint8;
    update_ref_delta: uint8;
    loop_filter_ref_deltas: array[0..STD_VIDEO_VP9_MAX_REF_FRAMES - 1] of int8;
    update_mode_delta: uint8;
    loop_filter_mode_deltas: array[0..STD_VIDEO_VP9_LOOP_FILTER_ADJUSTMENTS - 1] of int8;
  end;

  StdVideoVP9SegmentationFlags = bitpacked record
    segmentation_update_map: 0..1;
    segmentation_temporal_update: 0..1;
    segmentation_update_data: 0..1;
    segmentation_abs_or_delta_update: 0..1;
    reserved: 0..28;
  end;

  StdVideoVP9Segmentation = record
    flags: StdVideoVP9SegmentationFlags;
    segmentation_tree_probs: array[0..STD_VIDEO_VP9_MAX_SEGMENTATION_TREE_PROBS - 1] of uint8;
    segmentation_pred_prob: array[0..STD_VIDEO_VP9_MAX_SEGMENTATION_PRED_PROB - 1] of uint8;
    FeatureEnabled: array[0..STD_VIDEO_VP9_MAX_SEGMENTS - 1] of uint8;
    FeatureData: array[0..STD_VIDEO_VP9_MAX_SEGMENTS - 1, 0..STD_VIDEO_VP9_SEG_LVL_MAX - 1] of int16;
  end;

  // ─────────────────────────────────────────────────────────────────────────────
  //                        Tipi specifici per il DECODE
  // ─────────────────────────────────────────────────────────────────────────────

  StdVideoDecodeVP9PictureInfoFlags = bitpacked record
    error_resilient_mode: 0..1;
    intra_only: 0..1;
    allow_high_precision_mv: 0..1;
    refresh_frame_context: 0..1;
    frame_parallel_decoding_mode: 0..1;
    segmentation_enabled: 0..1;
    show_frame: 0..1;
    UsePrevFrameMvs: 0..1;
    reserved: 0..24;
  end;

  PStdVideoDecodeVP9PictureInfo = ^StdVideoDecodeVP9PictureInfo;

  StdVideoDecodeVP9PictureInfo = record
    flags: StdVideoDecodeVP9PictureInfoFlags;
    profile: StdVideoVP9Profile;
    frame_type: StdVideoVP9FrameType;
    frame_context_idx: uint8;
    reset_frame_context: uint8;
    refresh_frame_flags: uint8;
    ref_frame_sign_bias_mask: uint8;
    interpolation_filter: StdVideoVP9InterpolationFilter;
    base_q_idx: uint8;
    delta_q_y_dc: int8;
    delta_q_uv_dc: int8;
    delta_q_uv_ac: int8;
    tile_cols_log2: uint8;
    tile_rows_log2: uint8;
    reserved1: array[0..2] of uint16;
    pColorConfig: ^StdVideoVP9ColorConfig;
    pLoopFilter: ^StdVideoVP9LoopFilter;
    pSegmentation: ^StdVideoVP9Segmentation;
  end;

  // ─────────────────────────────────────────────────────────────────────────────
  //                           Estensione Vulkan ufficiale
  // ─────────────────────────────────────────────────────────────────────────────

const
  VK_STD_VULKAN_VIDEO_CODEC_VP9_DECODE_EXTENSION_NAME = 'VK_STD_vulkan_video_codec_vp9_decode';
