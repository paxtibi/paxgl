
const
  STD_VIDEO_H265_CPB_CNT_LIST_SIZE = 32;
  STD_VIDEO_H265_SUBLAYERS_LIST_SIZE = 7;
  STD_VIDEO_H265_SCALING_LIST_4X4_NUM_LISTS = 6;
  STD_VIDEO_H265_SCALING_LIST_4X4_NUM_ELEMENTS = 16;
  STD_VIDEO_H265_SCALING_LIST_8X8_NUM_LISTS = 6;
  STD_VIDEO_H265_SCALING_LIST_8X8_NUM_ELEMENTS = 64;
  STD_VIDEO_H265_SCALING_LIST_16X16_NUM_LISTS = 6;
  STD_VIDEO_H265_SCALING_LIST_16X16_NUM_ELEMENTS = 64;
  STD_VIDEO_H265_SCALING_LIST_32X32_NUM_LISTS = 2;
  STD_VIDEO_H265_SCALING_LIST_32X32_NUM_ELEMENTS = 64;
  STD_VIDEO_H265_CHROMA_QP_OFFSET_LIST_SIZE = 6;
  STD_VIDEO_H265_CHROMA_QP_OFFSETILE_COLS_LIST_SIZE = 19;
  STD_VIDEO_H265_CHROMA_QP_OFFSETILE_ROWS_LIST_SIZE = 21;
  STD_VIDEO_H265_PREDICTOR_PALETTE_COMPONENTS_LIST_SIZE = 3;
  STD_VIDEO_H265_PREDICTOR_PALETTE_COMP_ENTRIES_LIST_SIZE = 128;
  STD_VIDEO_H265_MAX_NUM_LIST_REF = 15;
  STD_VIDEO_H265_MAX_CHROMA_PLANES = 2;
  STD_VIDEO_H265_MAX_SHORTERM_REF_PIC_SETS = 64;
  STD_VIDEO_H265_MAX_DPB_SIZE = 16;
  STD_VIDEO_H265_MAX_LONG_TERM_REF_PICS_SPS = 32;
  STD_VIDEO_H265_MAX_LONG_TERM_PICS = 16;
  STD_VIDEO_H265_MAX_DELTA_POC = 48;
  STD_VIDEO_H265_NO_REFERENCE_PICTURE = $FF;

type
  StdVideoH265ChromaFormatIdc = (
    STD_VIDEO_H265_CHROMA_FORMAT_IDC_MONOCHROME = 0,
    STD_VIDEO_H265_CHROMA_FORMAT_IDC_420 = 1,
    STD_VIDEO_H265_CHROMA_FORMAT_IDC_422 = 2,
    STD_VIDEO_H265_CHROMA_FORMAT_IDC_444 = 3,
    STD_VIDEO_H265_CHROMA_FORMAT_IDC_INVALID = $7FFFFFFF,
    STD_VIDEO_H265_CHROMA_FORMAT_IDC_MAX_ENUM = $7FFFFFFF);

  StdVideoH265ProfileIdc = (
    STD_VIDEO_H265_PROFILE_IDC_MAIN = 1,
    STD_VIDEO_H265_PROFILE_IDC_MAIN_10 = 2,
    STD_VIDEO_H265_PROFILE_IDC_MAIN_STILL_PICTURE = 3,
    STD_VIDEO_H265_PROFILE_IDC_FORMAT_RANGE_EXTENSIONS = 4,
    STD_VIDEO_H265_PROFILE_IDC_SCC_EXTENSIONS = 9,
    STD_VIDEO_H265_PROFILE_IDC_INVALID = $7FFFFFFF,
    STD_VIDEO_H265_PROFILE_IDC_MAX_ENUM = $7FFFFFFF);

  StdVideoH265LevelIdc = (
    STD_VIDEO_H265_LEVEL_IDC_1_0 = 0,
    STD_VIDEO_H265_LEVEL_IDC_2_0 = 1,
    STD_VIDEO_H265_LEVEL_IDC_2_1 = 2,
    STD_VIDEO_H265_LEVEL_IDC_3_0 = 3,
    STD_VIDEO_H265_LEVEL_IDC_3_1 = 4,
    STD_VIDEO_H265_LEVEL_IDC_4_0 = 5,
    STD_VIDEO_H265_LEVEL_IDC_4_1 = 6,
    STD_VIDEO_H265_LEVEL_IDC_5_0 = 7,
    STD_VIDEO_H265_LEVEL_IDC_5_1 = 8,
    STD_VIDEO_H265_LEVEL_IDC_5_2 = 9,
    STD_VIDEO_H265_LEVEL_IDC_6_0 = 10,
    STD_VIDEO_H265_LEVEL_IDC_6_1 = 11,
    STD_VIDEO_H265_LEVEL_IDC_6_2 = 12,
    STD_VIDEO_H265_LEVEL_IDC_INVALID = $7FFFFFFF,
    STD_VIDEO_H265_LEVEL_IDC_MAX_ENUM = $7FFFFFFF);

  StdVideoH265SliceType = (
    STD_VIDEO_H265_SLICEYPE_B = 0,
    STD_VIDEO_H265_SLICEYPE_P = 1,
    STD_VIDEO_H265_SLICEYPE_I = 2,
    STD_VIDEO_H265_SLICEYPE_INVALID = $7FFFFFFF,
    STD_VIDEO_H265_SLICEYPE_MAX_ENUM = $7FFFFFFF);

  StdVideoH265PictureType = (
    STD_VIDEO_H265_PICTUREYPE_P = 0,
    STD_VIDEO_H265_PICTUREYPE_B = 1,
    STD_VIDEO_H265_PICTUREYPE_I = 2,
    STD_VIDEO_H265_PICTUREYPE_IDR = 3,
    STD_VIDEO_H265_PICTUREYPE_INVALID = $7FFFFFFF,
    STD_VIDEO_H265_PICTUREYPE_MAX_ENUM = $7FFFFFFF);

  StdVideoH265AspectRatioIdc = (
    STD_VIDEO_H265_ASPECT_RATIO_IDC_UNSPECIFIED = 0,
    STD_VIDEO_H265_ASPECT_RATIO_IDC_SQUARE = 1,
    STD_VIDEO_H265_ASPECT_RATIO_IDC_12_11 = 2,
    STD_VIDEO_H265_ASPECT_RATIO_IDC_10_11 = 3,
    STD_VIDEO_H265_ASPECT_RATIO_IDC_16_11 = 4,
    STD_VIDEO_H265_ASPECT_RATIO_IDC_40_33 = 5,
    STD_VIDEO_H265_ASPECT_RATIO_IDC_24_11 = 6,
    STD_VIDEO_H265_ASPECT_RATIO_IDC_20_11 = 7,
    STD_VIDEO_H265_ASPECT_RATIO_IDC_32_11 = 8,
    STD_VIDEO_H265_ASPECT_RATIO_IDC_80_33 = 9,
    STD_VIDEO_H265_ASPECT_RATIO_IDC_18_11 = 10,
    STD_VIDEO_H265_ASPECT_RATIO_IDC_15_11 = 11,
    STD_VIDEO_H265_ASPECT_RATIO_IDC_64_33 = 12,
    STD_VIDEO_H265_ASPECT_RATIO_IDC_160_99 = 13,
    STD_VIDEO_H265_ASPECT_RATIO_IDC_4_3 = 14,
    STD_VIDEO_H265_ASPECT_RATIO_IDC_3_2 = 15,
    STD_VIDEO_H265_ASPECT_RATIO_IDC_2_1 = 16,
    STD_VIDEO_H265_ASPECT_RATIO_IDC_EXTENDED_SAR = 255,
    STD_VIDEO_H265_ASPECT_RATIO_IDC_INVALID = $7FFFFFFF,
    STD_VIDEO_H265_ASPECT_RATIO_IDC_MAX_ENUM = $7FFFFFFF);
  PStdVideoH265DecPicBufMgr = ^StdVideoH265DecPicBufMgr;

  StdVideoH265DecPicBufMgr = record
    max_latency_increase_plus1: array [0..STD_VIDEO_H265_SUBLAYERS_LIST_SIZE - 1] of uint32;
    max_dec_pic_buffering_minus1: array [0..STD_VIDEO_H265_SUBLAYERS_LIST_SIZE - 1] of uint8;
    max_num_reorder_pics: array [0..STD_VIDEO_H265_SUBLAYERS_LIST_SIZE - 1] of uint8;
  end;
  PStdVideoH265SubLayerHrdParameters = ^StdVideoH265SubLayerHrdParameters;

  StdVideoH265SubLayerHrdParameters = record
    bit_rate_value_minus1: array [0..STD_VIDEO_H265_CPB_CNT_LIST_SIZE - 1] of uint32;
    cpb_size_value_minus1: array [0..STD_VIDEO_H265_CPB_CNT_LIST_SIZE - 1] of uint32;
    cpb_size_du_value_minus1: array [0..STD_VIDEO_H265_CPB_CNT_LIST_SIZE - 1] of uint32;
    bit_rate_du_value_minus1: array [0..STD_VIDEO_H265_CPB_CNT_LIST_SIZE - 1] of uint32;
    cbr_flag: uint32;
  end;

  StdVideoH265HrdFlags = bitpacked record
    nal_hrd_parameters_present_flag: 0..1;
    vcl_hrd_parameters_present_flag: 0..1;
    sub_pic_hrd_params_present_flag: 0..1;
    sub_pic_cpb_params_in_piciming_sei_flag: 0..1;
    fixed_pic_rate_general_flag: byte;
    fixed_pic_rate_within_cvs_flag: byte;
    low_delay_hrd_flag: byte;
  end;

  PStdVideoH265HrdParameters = ^StdVideoH265HrdParameters;

  StdVideoH265HrdParameters = record
    flags: StdVideoH265HrdFlags;
    tick_divisor_minus2: uint8;
    du_cpb_removal_delay_increment_length_minus1: uint8;
    dpb_output_delay_du_length_minus1: uint8;
    bit_rate_scale: uint8;
    cpb_size_scale: uint8;
    cpb_size_du_scale: uint8;
    initial_cpb_removal_delay_length_minus1: uint8;
    au_cpb_removal_delay_length_minus1: uint8;
    dpb_output_delay_length_minus1: uint8;
    cpb_cnt_minus1: array[0..STD_VIDEO_H265_SUBLAYERS_LIST_SIZE - 1] of uint8;
    elemental_duration_inc_minus1: array [0..STD_VIDEO_H265_SUBLAYERS_LIST_SIZE - 1] of uint16;
    reserved: array[0..2] of uint16;
    pSubLayerHrdParametersNal: PStdVideoH265SubLayerHrdParameters;
    pSubLayerHrdParametersVcl: PStdVideoH265SubLayerHrdParameters;
  end;

  PStdVideoH265VpsFlags = ^StdVideoH265VpsFlags;

  StdVideoH265VpsFlags = record
    vpsemporal_id_nesting_flag: 0..1;
    vps_sub_layer_ordering_info_present_flag: 0..1;
    vpsiming_info_present_flag: 0..1;
    vps_poc_proportionaloiming_flag: 0..1;
  end;

  PStdVideoH265ProfileTierLevelFlags = ^StdVideoH265ProfileTierLevelFlags;

  StdVideoH265ProfileTierLevelFlags = bitpacked record
    generalier_flag: 0..1;
    general_progressive_source_flag: 0..1;
    general_interlaced_source_flag: 0..1;
    general_non_packed_constraint_flag: 0..1;
    general_frame_only_constraint_flag: 0..1;
  end;
  PStdVideoH265ProfileTierLevel = ^StdVideoH265ProfileTierLevel;

  StdVideoH265ProfileTierLevel = record
    flags: StdVideoH265ProfileTierLevelFlags;
    general_profile_idc: StdVideoH265ProfileIdc;
    general_level_idc: StdVideoH265LevelIdc;
  end;

  PStdVideoH265VideoParameterSet = ^StdVideoH265VideoParameterSet;

  StdVideoH265VideoParameterSet = record
    flags: StdVideoH265VpsFlags;
    vps_video_parameter_set_id: uint8;
    vps_max_sub_layers_minus1: uint8;
    reserved1: uint8;
    reserved2: uint8;
    vps_num_units_inick: uint32;
    vpsime_scale: uint32;
    vps_numicks_poc_diff_one_minus1: uint32;
    reserved3: uint32;
    pDecPicBufMgr: PStdVideoH265DecPicBufMgr;
    pHrdParameters: PStdVideoH265HrdParameters;
    pProfileTierLevel: PStdVideoH265ProfileTierLevel;
  end;

  PStdVideoH265ScalingLists = ^StdVideoH265ScalingLists;

  StdVideoH265ScalingLists = record
    ScalingList4x4: array [0..STD_VIDEO_H265_SCALING_LIST_4X4_NUM_LISTS - 1, 0..STD_VIDEO_H265_SCALING_LIST_4X4_NUM_ELEMENTS - 1] of uint8;
    ScalingList8x8: array [0..STD_VIDEO_H265_SCALING_LIST_8X8_NUM_LISTS - 1, 0..STD_VIDEO_H265_SCALING_LIST_8X8_NUM_ELEMENTS - 1] of uint8;
    ScalingList16x16: array [0..STD_VIDEO_H265_SCALING_LIST_16X16_NUM_LISTS - 1, 0..STD_VIDEO_H265_SCALING_LIST_16X16_NUM_ELEMENTS - 1] of uint8;
    ScalingList32x32: array [0..STD_VIDEO_H265_SCALING_LIST_32X32_NUM_LISTS - 1, 0..STD_VIDEO_H265_SCALING_LIST_32X32_NUM_ELEMENTS - 1] of uint8;
    ScalingListDCCoef16x16: array [0..STD_VIDEO_H265_SCALING_LIST_16X16_NUM_LISTS - 1] of uint8;
    ScalingListDCCoef32x32: array [0..STD_VIDEO_H265_SCALING_LIST_32X32_NUM_LISTS - 1] of uint8;
  end;

  StdVideoH265SpsVuiFlags = bitpacked record
    aspect_ratio_info_present_flag: 0..1;
    overscan_info_present_flag: 0..1;
    overscan_appropriate_flag: 0..1;
    video_signalype_present_flag: 0..1;
    video_full_range_flag: 0..1;
    colour_description_present_flag: 0..1;
    chroma_loc_info_present_flag: 0..1;
    neutral_chroma_indication_flag: 0..1;
    field_seq_flag: 0..1;
    frame_field_info_present_flag: 0..1;
    default_display_window_flag: 0..1;
    vuiiming_info_present_flag: 0..1;
    vui_poc_proportionaloiming_flag: 0..1;
    vui_hrd_parameters_present_flag: 0..1;
    bitstream_restriction_flag: 0..1;
    tiles_fixed_structure_flag: 0..1;
    motion_vectors_over_pic_boundaries_flag: 0..1;
    restricted_ref_pic_lists_flag: 0..1;
  end;

  PStdVideoH265SequenceParameterSetVui = ^StdVideoH265SequenceParameterSetVui;

  StdVideoH265SequenceParameterSetVui = record
    flags: StdVideoH265SpsVuiFlags;
    aspect_ratio_idc: StdVideoH265AspectRatioIdc;
    sar_width: uint16;
    sar_height: uint16;
    video_format: uint8;
    colour_primaries: uint8;
    transfer_characteristics: uint8;
    matrix_coeffs: uint8;
    chroma_sample_locypeop_field: uint8;
    chroma_sample_locype_bottom_field: uint8;
    reserved1: uint8;
    reserved2: uint8;
    def_disp_win_left_offset: uint16;
    def_disp_win_right_offset: uint16;
    def_disp_winop_offset: uint16;
    def_disp_win_bottom_offset: uint16;
    vui_num_units_inick: uint32;
    vuiime_scale: uint32;
    vui_numicks_poc_diff_one_minus1: uint32;
    min_spatial_segmentation_idc: uint16;
    reserved3: uint16;
    max_bytes_per_pic_denom: uint8;
    max_bits_per_min_cu_denom: uint8;
    log2_max_mv_length_horizontal: uint8;
    log2_max_mv_length_vertical: uint8;
    pHrdParameters: PStdVideoH265HrdParameters;
  end;

  PStdVideoH265PredictorPaletteEntries = ^StdVideoH265PredictorPaletteEntries;

  StdVideoH265PredictorPaletteEntries = record
    PredictorPaletteEntries: array[0..STD_VIDEO_H265_PREDICTOR_PALETTE_COMPONENTS_LIST_SIZE - 1, 0..STD_VIDEO_H265_PREDICTOR_PALETTE_COMP_ENTRIES_LIST_SIZE - 1] of uint16;
  end;

  StdVideoH265SpsFlags = bitpacked record
    spsemporal_id_nesting_flag: 0..1;
    separate_colour_plane_flag: 0..1;
    conformance_window_flag: 0..1;
    sps_sub_layer_ordering_info_present_flag: 0..1;
    scaling_list_enabled_flag: 0..1;
    sps_scaling_list_data_present_flag: 0..1;
    amp_enabled_flag: 0..1;
    sample_adaptive_offset_enabled_flag: 0..1;
    pcm_enabled_flag: 0..1;
    pcm_loop_filter_disabled_flag: 0..1;
    longerm_ref_pics_present_flag: 0..1;
    spsemporal_mvp_enabled_flag: 0..1;
    strong_intra_smoothing_enabled_flag: 0..1;
    vui_parameters_present_flag: 0..1;
    sps_extension_present_flag: 0..1;
    sps_range_extension_flag: 0..1;
    transform_skip_rotation_enabled_flag: 0..1;
    transform_skip_context_enabled_flag: 0..1;
    implicit_rdpcm_enabled_flag: 0..1;
    explicit_rdpcm_enabled_flag: 0..1;
    extended_precision_processing_flag: 0..1;
    intra_smoothing_disabled_flag: 0..1;
    high_precision_offsets_enabled_flag: 0..1;
    persistent_rice_adaptation_enabled_flag: 0..1;
    cabac_bypass_alignment_enabled_flag: 0..1;
    sps_scc_extension_flag: 0..1;
    sps_curr_pic_ref_enabled_flag: 0..1;
    palette_mode_enabled_flag: 0..1;
    sps_palette_predictor_initializers_present_flag: 0..1;
    intra_boundary_filtering_disabled_flag: 0..1;
  end;

  StdVideoH265ShortTermRefPicSetFlags = bitpacked record
    inter_ref_pic_set_prediction_flag: 0..1;
    delta_rps_sign: 0..1;
  end;

  PStdVideoH265ShortTermRefPicSet = ^StdVideoH265ShortTermRefPicSet;

  StdVideoH265ShortTermRefPicSet = record
    flags: StdVideoH265ShortTermRefPicSetFlags;
    delta_idx_minus1: uint32;
    use_delta_flag: uint16;
    abs_delta_rps_minus1: uint16;
    used_by_curr_pic_flag: uint16;
    used_by_curr_pic_s0_flag: uint16;
    used_by_curr_pic_s1_flag: uint16;
    reserved1: uint16;
    reserved2: uint8;
    reserved3: uint8;
    num_negative_pics: uint8;
    num_positive_pics: uint8;
    delta_poc_s0_minus1: array[0..STD_VIDEO_H265_MAX_DPB_SIZE - 1] of uint16;
    delta_poc_s1_minus1: array[0..STD_VIDEO_H265_MAX_DPB_SIZE - 1] of uint16;
  end;

  PStdVideoH265LongTermRefPicsSps = ^StdVideoH265LongTermRefPicsSps;

  StdVideoH265LongTermRefPicsSps = record
    used_by_curr_pic_lt_sps_flag: uint32;
    lt_ref_pic_poc_lsb_sps: array[0..STD_VIDEO_H265_MAX_LONG_TERM_REF_PICS_SPS - 1] of uint32;
  end;

  PStdVideoH265SequenceParameterSet = ^StdVideoH265SequenceParameterSet;

  StdVideoH265SequenceParameterSet = record
    flags: StdVideoH265SpsFlags;
    chroma_format_idc: StdVideoH265ChromaFormatIdc;
    pic_width_in_luma_samples: uint32;
    pic_height_in_luma_samples: uint32;
    sps_video_parameter_set_id: uint8;
    sps_max_sub_layers_minus1: uint8;
    sps_seq_parameter_set_id: uint8;
    bit_depth_luma_minus8: uint8;
    bit_depth_chroma_minus8: uint8;
    log2_max_pic_order_cnt_lsb_minus4: uint8;
    log2_min_luma_coding_block_size_minus3: uint8;
    log2_diff_max_min_luma_coding_block_size: uint8;
    log2_min_lumaransform_block_size_minus2: uint8;
    log2_diff_max_min_lumaransform_block_size: uint8;
    maxransform_hierarchy_depth_inter: uint8;
    maxransform_hierarchy_depth_intra: uint8;
    num_shorterm_ref_pic_sets: uint8;
    num_longerm_ref_pics_sps: uint8;
    pcm_sample_bit_depth_luma_minus1: uint8;
    pcm_sample_bit_depth_chroma_minus1: uint8;
    log2_min_pcm_luma_coding_block_size_minus3: uint8;
    log2_diff_max_min_pcm_luma_coding_block_size: uint8;
    reserved1: uint8;
    reserved2: uint8;
    palette_max_size: uint8;
    delta_palette_max_predictor_size: uint8;
    motion_vector_resolution_control_idc: uint8;
    sps_num_palette_predictor_initializers_minus1: uint8;
    conf_win_left_offset: uint32;
    conf_win_right_offset: uint32;
    conf_winop_offset: uint32;
    conf_win_bottom_offset: uint32;
    pProfileTierLevel: PStdVideoH265ProfileTierLevel;
    pDecPicBufMgr: PStdVideoH265DecPicBufMgr;
    pScalingLists: PStdVideoH265ScalingLists;
    pShortTermRefPicSet: PStdVideoH265ShortTermRefPicSet;
    pLongTermRefPicsSps: PStdVideoH265LongTermRefPicsSps;
    pSequenceParameterSetVui: PStdVideoH265SequenceParameterSetVui;
    pPredictorPaletteEntries: PStdVideoH265PredictorPaletteEntries;
  end;

  StdVideoH265PpsFlags = bitpacked record
    dependent_slice_segments_enabled_flag: 0..1;
    output_flag_present_flag: 0..1;
    sign_data_hiding_enabled_flag: 0..1;
    cabac_init_present_flag: 0..1;
    constrained_intra_pred_flag: 0..1;
    transform_skip_enabled_flag: 0..1;
    cu_qp_delta_enabled_flag: 0..1;
    pps_slice_chroma_qp_offsets_present_flag: 0..1;
    weighted_pred_flag: 0..1;
    weighted_bipred_flag: 0..1;
    transquant_bypass_enabled_flag: 0..1;
    tiles_enabled_flag: 0..1;
    entropy_coding_sync_enabled_flag: 0..1;
    uniform_spacing_flag: 0..1;
    loop_filter_acrossiles_enabled_flag: 0..1;
    pps_loop_filter_across_slices_enabled_flag: 0..1;
    deblocking_filter_control_present_flag: 0..1;
    deblocking_filter_override_enabled_flag: 0..1;
    pps_deblocking_filter_disabled_flag: 0..1;
    pps_scaling_list_data_present_flag: 0..1;
    lists_modification_present_flag: 0..1;
    slice_segment_header_extension_present_flag: 0..1;
    pps_extension_present_flag: 0..1;
    cross_component_prediction_enabled_flag: 0..1;
    chroma_qp_offset_list_enabled_flag: 0..1;
    pps_curr_pic_ref_enabled_flag: 0..1;
    residual_adaptive_colourransform_enabled_flag: 0..1;
    pps_slice_act_qp_offsets_present_flag: 0..1;
    pps_palette_predictor_initializers_present_flag: 0..1;
    monochrome_palette_flag: 0..1;
    pps_range_extension_flag: 0..1;
  end;

  PStdVideoH265PictureParameterSet = ^StdVideoH265PictureParameterSet;

  StdVideoH265PictureParameterSet = record
    flags: StdVideoH265PpsFlags;
    pps_pic_parameter_set_id: uint8;
    pps_seq_parameter_set_id: uint8;
    sps_video_parameter_set_id: uint8;
    num_extra_slice_header_bits: uint8;
    num_ref_idx_l0_default_active_minus1: uint8;
    num_ref_idx_l1_default_active_minus1: uint8;
    init_qp_minus26: int8;
    diff_cu_qp_delta_depth: uint8;
    pps_cb_qp_offset: int8;
    pps_cr_qp_offset: int8;
    pps_beta_offset_div2: int8;
    ppsc_offset_div2: int8;
    log2_parallel_merge_level_minus2: uint8;
    log2_maxransform_skip_block_size_minus2: uint8;
    diff_cu_chroma_qp_offset_depth: uint8;
    chroma_qp_offset_list_len_minus1: uint8;
    cb_qp_offset_list: array[0..STD_VIDEO_H265_CHROMA_QP_OFFSET_LIST_SIZE - 1] of int8;
    cr_qp_offset_list: array[0..STD_VIDEO_H265_CHROMA_QP_OFFSET_LIST_SIZE - 1] of int8;
    log2_sao_offset_scale_luma: uint8;
    log2_sao_offset_scale_chroma: uint8;
    pps_act_y_qp_offset_plus5: int8;
    pps_act_cb_qp_offset_plus5: int8;
    pps_act_cr_qp_offset_plus3: int8;
    pps_num_palette_predictor_initializers: uint8;
    luma_bit_depth_entry_minus8: uint8;
    chroma_bit_depth_entry_minus8: uint8;
    numile_columns_minus1: uint8;
    numile_rows_minus1: uint8;
    reserved1: uint8;
    reserved2: uint8;
    column_width_minus1: array[0..STD_VIDEO_H265_CHROMA_QP_OFFSETILE_COLS_LIST_SIZE - 1] of uint16;
    row_height_minus1: array[0..STD_VIDEO_H265_CHROMA_QP_OFFSETILE_ROWS_LIST_SIZE - 1] of uint16;
    reserved3: uint32;
    pScalingLists: PStdVideoH265ScalingLists;
    pPredictorPaletteEntries: PStdVideoH265PredictorPaletteEntries;
  end;


const
  VK_STD_VULKAN_VIDEO_CODEC_H265_DECODE_EXTENSION_NAME = 'VK_STD_vulkan_video_codec_h265_decode';
  STD_VIDEO_DECODE_H265_REF_PIC_SET_LIST_SIZE = 8;

type
  StdVideoDecodeH265PictureInfoFlags = bitpacked record
    IrapPicFlag: 0..1;
    IdrPicFlag: 0..1;
    IsReference: 0..1;
    short_term_ref_pic_set_sps_flag: 0..1;
  end;

  StdVideoDecodeH265PictureInfo = record
    flags: StdVideoDecodeH265PictureInfoFlags;
    sps_video_parameter_set_id: uint8;
    pps_seq_parameter_set_id: uint8;
    pps_pic_parameter_set_id: uint8;
    NumDeltaPocsOfRefRpsIdx: uint8;
    PicOrderCntVal: int32;
    NumBitsForSTRefPicSetInSlice: uint16;
    reserved: uint16;
    RefPicSetStCurrBefore: array[0..STD_VIDEO_DECODE_H265_REF_PIC_SET_LIST_SIZE - 1] of uint8;
    RefPicSetStCurrAfter: array[0..STD_VIDEO_DECODE_H265_REF_PIC_SET_LIST_SIZE - 1] of uint8;
    RefPicSetLtCurr: array[0..STD_VIDEO_DECODE_H265_REF_PIC_SET_LIST_SIZE - 1] of uint8;
  end;

  StdVideoDecodeH265ReferenceInfoFlags = bitpacked record
    used_for_long_term_reference: 0..1;
    unused_for_reference: 0..1;
  end;

  StdVideoDecodeH265ReferenceInfo = record
    flags: StdVideoDecodeH265ReferenceInfoFlags;
    PicOrderCntVal: int32;
  end;

const

  VK_STD_VULKAN_VIDEO_CODEC_H265_ENCODE_EXTENSION_NAME = 'VK_STD_vulkan_video_codec_h265_encode';

type
  StdVideoEncodeH265WeightTableFlags = record
    luma_weight_l0_flag: uint16;
    chroma_weight_l0_flag: uint16;
    luma_weight_l1_flag: uint16;
    chroma_weight_l1_flag: uint16;
  end;

  StdVideoEncodeH265WeightTable = packed record
    flags: StdVideoEncodeH265WeightTableFlags;
    luma_log2_weight_denom: uint8;
    delta_chroma_log2_weight_denom: int8;
    delta_luma_weight_l0: array[0..STD_VIDEO_H265_MAX_NUM_LIST_REF - 1] of int8;
    luma_offset_l0: array[0..STD_VIDEO_H265_MAX_NUM_LIST_REF - 1] of int8;
    delta_chroma_weight_l0: array[0..STD_VIDEO_H265_MAX_NUM_LIST_REF - 1, 0..STD_VIDEO_H265_MAX_CHROMA_PLANES - 1] of int8;
    delta_chroma_offset_l0: array[0..STD_VIDEO_H265_MAX_NUM_LIST_REF - 1, 0..STD_VIDEO_H265_MAX_CHROMA_PLANES - 1] of int8;
    delta_luma_weight_l1: array[0..STD_VIDEO_H265_MAX_NUM_LIST_REF - 1] of int8;
    luma_offset_l1: array[0..STD_VIDEO_H265_MAX_NUM_LIST_REF - 1] of int8;
    delta_chroma_weight_l1: array[0..STD_VIDEO_H265_MAX_NUM_LIST_REF - 1, 0..STD_VIDEO_H265_MAX_CHROMA_PLANES - 1] of int8;
    delta_chroma_offset_l1: array[0..STD_VIDEO_H265_MAX_NUM_LIST_REF - 1, 0..STD_VIDEO_H265_MAX_CHROMA_PLANES - 1] of int8;
  end;

  StdVideoEncodeH265SliceSegmentHeaderFlags = bitpacked record
    case byte of
      0: (Value: uint32);
      1: (
        first_slice_segment_in_pic_flag: 0..1;
        dependent_slice_segment_flag: 0..1;
        slice_sao_luma_flag: 0..1;
        slice_sao_chroma_flag: 0..1;
        num_ref_idx_active_override_flag: 0..1;
        mvd_l1_zero_flag: 0..1;
        cabac_init_flag: 0..1;
        cu_chroma_qp_offset_enabled_flag: 0..1;
        deblocking_filter_override_flag: 0..1;
        slice_deblocking_filter_disabled_flag: 0..1;
        collocated_from_l0_flag: 0..1;
        slice_loop_filter_across_slices_enabled_flag: 0..1;
        reserved: 0..(1 shl 20) - 1;
      );
  end;

  StdVideoEncodeH265SliceSegmentHeader = packed record
    flags: StdVideoEncodeH265SliceSegmentHeaderFlags;
    slice_type: StdVideoH265SliceType;     // ← devi definire questo tipo enumerato prima
    slice_segment_address: uint32;
    collocated_ref_idx: uint8;
    MaxNumMergeCand: uint8;
    slice_cb_qp_offset: int8;
    slice_cr_qp_offset: int8;
    slice_beta_offset_div2: int8;
    slice_tc_offset_div2: int8;
    slice_act_y_qp_offset: int8;
    slice_act_cb_qp_offset: int8;
    slice_act_cr_qp_offset: int8;
    slice_qp_delta: int8;
    reserved1: uint16;
    pWeightTable: ^StdVideoEncodeH265WeightTable;  // pointer to const in C → pointer in Pascal
  end;

  StdVideoEncodeH265ReferenceListsInfoFlags = bitpacked record
    case byte of
      0: (Value: uint32);
      1: (
        ref_pic_list_modification_flag_l0: 0..1;
        ref_pic_list_modification_flag_l1: 0..1;
        reserved: 0..(1 shl 30) - 1;)
  end;

  StdVideoEncodeH265ReferenceListsInfo = packed record
    flags: StdVideoEncodeH265ReferenceListsInfoFlags;
    num_ref_idx_l0_active_minus1: uint8;
    num_ref_idx_l1_active_minus1: uint8;
    RefPicList0: array[0..STD_VIDEO_H265_MAX_NUM_LIST_REF - 1] of uint8;
    RefPicList1: array[0..STD_VIDEO_H265_MAX_NUM_LIST_REF - 1] of uint8;
    list_entry_l0: array[0..STD_VIDEO_H265_MAX_NUM_LIST_REF - 1] of uint8;
    list_entry_l1: array[0..STD_VIDEO_H265_MAX_NUM_LIST_REF - 1] of uint8;
  end;

  StdVideoEncodeH265PictureInfoFlags = packed record
    case byte of
      0: (Value: uint32);
      1: (
        is_reference: 0..1 {BITS 1};
        IrapPicFlag: 0..1 {BITS 1};
        used_for_long_term_reference: 0..1 {BITS 1};
        discardable_flag: 0..1 {BITS 1};
        cross_layer_bla_flag: 0..1 {BITS 1};
        pic_output_flag: 0..1 {BITS 1};
        no_output_of_prior_pics_flag: 0..1 {BITS 1};
        short_term_ref_pic_set_sps_flag: 0..1 {BITS 1};
        slice_temporal_mvp_enabled_flag: 0..1 {BITS 1};
        reserved: 0..(1 shl 23) - 1);
  end;

  StdVideoEncodeH265LongTermRefPics = packed record
    num_long_term_sps: uint8;
    num_long_term_pics: uint8;
    lt_idx_sps: array[0..STD_VIDEO_H265_MAX_LONG_TERM_REF_PICS_SPS - 1] of uint8;
    poc_lsb_lt: array[0..STD_VIDEO_H265_MAX_LONG_TERM_PICS - 1] of uint8;
    used_by_curr_pic_lt_flag: uint16;
    delta_poc_msb_present_flag: array[0..STD_VIDEO_H265_MAX_DELTA_POC - 1] of uint8;
    delta_poc_msb_cycle_lt: array[0..STD_VIDEO_H265_MAX_DELTA_POC - 1] of uint8;
  end;

  StdVideoEncodeH265PictureInfo = record
    flags: StdVideoEncodeH265PictureInfoFlags;
    pic_type: StdVideoH265PictureType;
    sps_video_parameter_set_id: uint8;
    pps_seq_parameter_set_id: uint8;
    pps_pic_parameter_set_id: uint8;
    short_term_ref_pic_set_idx: uint8;
    PicOrderCntVal: int32;
    TemporalId: uint8;
    reserved1: array[0..6] of uint8;
    pRefLists: ^StdVideoEncodeH265ReferenceListsInfo;
    pShortTermRefPicSet: ^StdVideoH265ShortTermRefPicSet;
    pLongTermRefPics: ^StdVideoEncodeH265LongTermRefPics;
  end;

  StdVideoEncodeH265ReferenceInfoFlags = bitpacked record
    used_for_long_term_reference: 0..1;
    unused_for_reference: 0..1;
    reserved: 0..(1 shl 30) - 1;
  end;

  StdVideoEncodeH265ReferenceInfo = record
    flags: StdVideoEncodeH265ReferenceInfoFlags;
    pic_type: StdVideoH265PictureType;
    PicOrderCntVal: int32;
    TemporalId: uint8;
  end;



implementation

end.
