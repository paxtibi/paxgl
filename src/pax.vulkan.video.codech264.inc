const
  STD_VIDEO_H264_CPB_CNT_LIST_SIZE = 32;
  STD_VIDEO_H264_SCALING_LIST_4X4_NUM_LISTS = 6;
  STD_VIDEO_H264_SCALING_LIST_4X4_NUM_ELEMENTS = 16;
  STD_VIDEO_H264_SCALING_LIST_8X8_NUM_LISTS = 6;
  STD_VIDEO_H264_SCALING_LIST_8X8_NUM_ELEMENTS = 64;
  STD_VIDEO_H264_MAX_NUM_LIST_REF = 32;
  STD_VIDEO_H264_MAX_CHROMA_PLANES = 2;
  STD_VIDEO_H264_NO_REFERENCE_PICTURE = $FF;

type
  StdVideoH264ChromaFormatIdc = (
    STD_VIDEO_H264_CHROMA_FORMAT_IDC_MONOCHROME = 0,
    STD_VIDEO_H264_CHROMA_FORMAT_IDC_420 = 1,
    STD_VIDEO_H264_CHROMA_FORMAT_IDC_422 = 2,
    STD_VIDEO_H264_CHROMA_FORMAT_IDC_444 = 3,
    STD_VIDEO_H264_CHROMA_FORMAT_IDC_INVALID = $7FFFFFFF,
    STD_VIDEO_H264_CHROMA_FORMAT_IDC_MAX_ENUM = $7FFFFFFF);

  StdVideoH264ProfileIdc = (
    STD_VIDEO_H264_PROFILE_IDC_BASELINE = 66,
    STD_VIDEO_H264_PROFILE_IDC_MAIN = 77,
    STD_VIDEO_H264_PROFILE_IDC_HIGH = 100,
    STD_VIDEO_H264_PROFILE_IDC_HIGH_444_PREDICTIVE = 244,
    STD_VIDEO_H264_PROFILE_IDC_INVALID = $7FFFFFFF,
    STD_VIDEO_H264_PROFILE_IDC_MAX_ENUM = $7FFFFFFF);

  StdVideoH264LevelIdc = (
    STD_VIDEO_H264_LEVEL_IDC_1_0 = 0,
    STD_VIDEO_H264_LEVEL_IDC_1_1 = 1,
    STD_VIDEO_H264_LEVEL_IDC_1_2 = 2,
    STD_VIDEO_H264_LEVEL_IDC_1_3 = 3,
    STD_VIDEO_H264_LEVEL_IDC_2_0 = 4,
    STD_VIDEO_H264_LEVEL_IDC_2_1 = 5,
    STD_VIDEO_H264_LEVEL_IDC_2_2 = 6,
    STD_VIDEO_H264_LEVEL_IDC_3_0 = 7,
    STD_VIDEO_H264_LEVEL_IDC_3_1 = 8,
    STD_VIDEO_H264_LEVEL_IDC_3_2 = 9,
    STD_VIDEO_H264_LEVEL_IDC_4_0 = 10,
    STD_VIDEO_H264_LEVEL_IDC_4_1 = 11,
    STD_VIDEO_H264_LEVEL_IDC_4_2 = 12,
    STD_VIDEO_H264_LEVEL_IDC_5_0 = 13,
    STD_VIDEO_H264_LEVEL_IDC_5_1 = 14,
    STD_VIDEO_H264_LEVEL_IDC_5_2 = 15,
    STD_VIDEO_H264_LEVEL_IDC_6_0 = 16,
    STD_VIDEO_H264_LEVEL_IDC_6_1 = 17,
    STD_VIDEO_H264_LEVEL_IDC_6_2 = 18,
    STD_VIDEO_H264_LEVEL_IDC_INVALID = $7FFFFFFF,
    STD_VIDEO_H264_LEVEL_IDC_MAX_ENUM = $7FFFFFFF);

  StdVideoH264PocType = (
    STD_VIDEO_H264_POCYPE_0 = 0,
    STD_VIDEO_H264_POCYPE_1 = 1,
    STD_VIDEO_H264_POCYPE_2 = 2,
    STD_VIDEO_H264_POCYPE_INVALID = $7FFFFFFF,
    STD_VIDEO_H264_POCYPE_MAX_ENUM = $7FFFFFFF);

  StdVideoH264AspectRatioIdc = (
    STD_VIDEO_H264_ASPECT_RATIO_IDC_UNSPECIFIED = 0,
    STD_VIDEO_H264_ASPECT_RATIO_IDC_SQUARE = 1,
    STD_VIDEO_H264_ASPECT_RATIO_IDC_12_11 = 2,
    STD_VIDEO_H264_ASPECT_RATIO_IDC_10_11 = 3,
    STD_VIDEO_H264_ASPECT_RATIO_IDC_16_11 = 4,
    STD_VIDEO_H264_ASPECT_RATIO_IDC_40_33 = 5,
    STD_VIDEO_H264_ASPECT_RATIO_IDC_24_11 = 6,
    STD_VIDEO_H264_ASPECT_RATIO_IDC_20_11 = 7,
    STD_VIDEO_H264_ASPECT_RATIO_IDC_32_11 = 8,
    STD_VIDEO_H264_ASPECT_RATIO_IDC_80_33 = 9,
    STD_VIDEO_H264_ASPECT_RATIO_IDC_18_11 = 10,
    STD_VIDEO_H264_ASPECT_RATIO_IDC_15_11 = 11,
    STD_VIDEO_H264_ASPECT_RATIO_IDC_64_33 = 12,
    STD_VIDEO_H264_ASPECT_RATIO_IDC_160_99 = 13,
    STD_VIDEO_H264_ASPECT_RATIO_IDC_4_3 = 14,
    STD_VIDEO_H264_ASPECT_RATIO_IDC_3_2 = 15,
    STD_VIDEO_H264_ASPECT_RATIO_IDC_2_1 = 16,
    STD_VIDEO_H264_ASPECT_RATIO_IDC_EXTENDED_SAR = 255,
    STD_VIDEO_H264_ASPECT_RATIO_IDC_INVALID = $7FFFFFFF,
    STD_VIDEO_H264_ASPECT_RATIO_IDC_MAX_ENUM = $7FFFFFFF);

  StdVideoH264WeightedBipredIdc = (
    STD_VIDEO_H264_WEIGHTED_BIPRED_IDC_DEFAULT = 0,
    STD_VIDEO_H264_WEIGHTED_BIPRED_IDC_EXPLICIT = 1,
    STD_VIDEO_H264_WEIGHTED_BIPRED_IDC_IMPLICIT = 2,
    STD_VIDEO_H264_WEIGHTED_BIPRED_IDC_INVALID = $7FFFFFFF,
    STD_VIDEO_H264_WEIGHTED_BIPRED_IDC_MAX_ENUM = $7FFFFFFF);

  StdVideoH264ModificationOfPicNumsIdc = (
    STD_VIDEO_H264_MODIFICATION_OF_PIC_NUMS_IDC_SHORTERM_SUBTRACT = 0,
    STD_VIDEO_H264_MODIFICATION_OF_PIC_NUMS_IDC_SHORTERM_ADD = 1,
    STD_VIDEO_H264_MODIFICATION_OF_PIC_NUMS_IDC_LONGERM = 2,
    STD_VIDEO_H264_MODIFICATION_OF_PIC_NUMS_IDC_END = 3,
    STD_VIDEO_H264_MODIFICATION_OF_PIC_NUMS_IDC_INVALID = $7FFFFFFF,
    STD_VIDEO_H264_MODIFICATION_OF_PIC_NUMS_IDC_MAX_ENUM = $7FFFFFFF);

  StdVideoH264MemMgmtControlOp = (
    STD_VIDEO_H264_MEM_MGMT_CONTROL_OP_END = 0,
    STD_VIDEO_H264_MEM_MGMT_CONTROL_OP_UNMARK_SHORTERM = 1,
    STD_VIDEO_H264_MEM_MGMT_CONTROL_OP_UNMARK_LONGERM = 2,
    STD_VIDEO_H264_MEM_MGMT_CONTROL_OP_MARK_LONGERM = 3,
    STD_VIDEO_H264_MEM_MGMT_CONTROL_OP_SET_MAX_LONGERM_INDEX = 4,
    STD_VIDEO_H264_MEM_MGMT_CONTROL_OP_UNMARK_ALL = 5,
    STD_VIDEO_H264_MEM_MGMT_CONTROL_OP_MARK_CURRENT_AS_LONGERM = 6,
    STD_VIDEO_H264_MEM_MGMT_CONTROL_OP_INVALID = $7FFFFFFF,
    STD_VIDEO_H264_MEM_MGMT_CONTROL_OP_MAX_ENUM = $7FFFFFFF);

  StdVideoH264CabacInitIdc = (
    STD_VIDEO_H264_CABAC_INIT_IDC_0 = 0,
    STD_VIDEO_H264_CABAC_INIT_IDC_1 = 1,
    STD_VIDEO_H264_CABAC_INIT_IDC_2 = 2,
    STD_VIDEO_H264_CABAC_INIT_IDC_INVALID = $7FFFFFFF,
    STD_VIDEO_H264_CABAC_INIT_IDC_MAX_ENUM = $7FFFFFFF);

  StdVideoH264DisableDeblockingFilterIdc = (
    STD_VIDEO_H264_DISABLE_DEBLOCKING_FILTER_IDC_DISABLED = 0,
    STD_VIDEO_H264_DISABLE_DEBLOCKING_FILTER_IDC_ENABLED = 1,
    STD_VIDEO_H264_DISABLE_DEBLOCKING_FILTER_IDC_PARTIAL = 2,
    STD_VIDEO_H264_DISABLE_DEBLOCKING_FILTER_IDC_INVALID = $7FFFFFFF,
    STD_VIDEO_H264_DISABLE_DEBLOCKING_FILTER_IDC_MAX_ENUM = $7FFFFFFF);

  StdVideoH264SliceType = (
    STD_VIDEO_H264_SLICEYPE_P = 0,
    STD_VIDEO_H264_SLICEYPE_B = 1,
    STD_VIDEO_H264_SLICEYPE_I = 2,
    STD_VIDEO_H264_SLICEYPE_INVALID = $7FFFFFFF,
    STD_VIDEO_H264_SLICEYPE_MAX_ENUM = $7FFFFFFF);

  StdVideoH264PictureType = (
    STD_VIDEO_H264_PICTUREYPE_P = 0,
    STD_VIDEO_H264_PICTUREYPE_B = 1,
    STD_VIDEO_H264_PICTUREYPE_I = 2,
    STD_VIDEO_H264_PICTUREYPE_IDR = 5,
    STD_VIDEO_H264_PICTUREYPE_INVALID = $7FFFFFFF,
    STD_VIDEO_H264_PICTUREYPE_MAX_ENUM = $7FFFFFFF);

  StdVideoH264NonVclNaluType = (
    STD_VIDEO_H264_NON_VCL_NALUYPE_SPS = 0,
    STD_VIDEO_H264_NON_VCL_NALUYPE_PPS = 1,
    STD_VIDEO_H264_NON_VCL_NALUYPE_AUD = 2,
    STD_VIDEO_H264_NON_VCL_NALUYPE_PREFIX = 3,
    STD_VIDEO_H264_NON_VCL_NALUYPE_END_OF_SEQUENCE = 4,
    STD_VIDEO_H264_NON_VCL_NALUYPE_END_OF_STREAM = 5,
    STD_VIDEO_H264_NON_VCL_NALUYPE_PRECODED = 6,
    STD_VIDEO_H264_NON_VCL_NALUYPE_INVALID = $7FFFFFFF,
    STD_VIDEO_H264_NON_VCL_NALUYPE_MAX_ENUM = $7FFFFFFF);

  StdVideoH264SpsVuiFlags = bitpacked record
    aspect_ratio_info_present_flag: 0..1;
    overscan_info_present_flag: 0..1;
    overscan_appropriate_flag: 0..1;
    video_signalype_present_flag: 0..1;
    video_full_range_flag: 0..1;
    color_description_present_flag: 0..1;
    chroma_loc_info_present_flag: 0..1;
    timing_info_present_flag: 0..1;
    fixed_frame_rate_flag: 0..1;
    bitstream_restriction_flag: 0..1;
    nal_hrd_parameters_present_flag: 0..1;
    vcl_hrd_parameters_present_flag: 0..1;
  end;

  PStdVideoH264HrdParameters = ^StdVideoH264HrdParameters;

  StdVideoH264HrdParameters = record
    cpb_cnt_minus1: uint8;
    bit_rate_scale: uint8;
    cpb_size_scale: uint8;
    reserved1: uint8;
    bit_rate_value_minus1: array [0..STD_VIDEO_H264_CPB_CNT_LIST_SIZE - 1] of uint32;
    cpb_size_value_minus1: array [0..STD_VIDEO_H264_CPB_CNT_LIST_SIZE - 1] of uint32;
    cbr_flag: array[0..STD_VIDEO_H264_CPB_CNT_LIST_SIZE - 1] of uint8;
    initial_cpb_removal_delay_length_minus1: uint32;
    cpb_removal_delay_length_minus1: uint32;
    dpb_output_delay_length_minus1: uint32;
    time_offset_length: uint32;
  end;

  StdVideoH264SequenceParameterSetVui = record
    flags: StdVideoH264SpsVuiFlags;
    aspect_ratio_idc: StdVideoH264AspectRatioIdc;
    sar_width: uint16;
    sar_height: uint16;
    video_format: uint8;
    colour_primaries: uint8;
    transfer_characteristics: uint8;
    matrix_coefficients: uint8;
    num_units_inick: uint32;
    time_scale: uint32;
    max_num_reorder_frames: uint8;
    max_dec_frame_buffering: uint8;
    chroma_sample_locypeop_field: uint8;
    chroma_sample_locype_bottom_field: uint8;
    reserved1: uint32;
    pHrdParameters: PStdVideoH264HrdParameters;
  end;

  PStdVideoH264SpsFlags = ^StdVideoH264SpsFlags;

  StdVideoH264SpsFlags = record
    constraint_set0_flag: 0..1;
    constraint_set1_flag: 0..1;
    constraint_set2_flag: 0..1;
    constraint_set3_flag: 0..1;
    constraint_set4_flag: 0..1;
    constraint_set5_flag: 0..1;
    direct_8x8_inference_flag: 0..1;
    mb_adaptive_frame_field_flag: 0..1;
    frame_mbs_only_flag: 0..1;
    delta_pic_order_always_zero_flag: 0..1;
    separate_colour_plane_flag: 0..1;
    gaps_in_frame_num_value_allowed_flag: 0..1;
    qpprime_y_zeroransform_bypass_flag: 0..1;
    frame_cropping_flag: 0..1;
    seq_scaling_matrix_present_flag: 0..1;
    vui_parameters_present_flag: 0..1;
  end;

  StdVideoH264ScalingLists = record
    scaling_list_present_mask: uint16;
    use_default_scaling_matrix_mask: uint16;
    ScalingList4x4: array[0..STD_VIDEO_H264_SCALING_LIST_4X4_NUM_LISTS - 1, 0..STD_VIDEO_H264_SCALING_LIST_4X4_NUM_ELEMENTS - 1] of uint8;
    ScalingList8x8: array [0..STD_VIDEO_H264_SCALING_LIST_8X8_NUM_LISTS - 1, 0..STD_VIDEO_H264_SCALING_LIST_8X8_NUM_ELEMENTS - 1] of uint8;
  end;

  StdVideoH264SequenceParameterSet = record
    flags: StdVideoH264SpsFlags;
    profile_idc: StdVideoH264ProfileIdc;
    level_idc: StdVideoH264LevelIdc;
    chroma_format_idc: StdVideoH264ChromaFormatIdc;
    seq_parameter_set_id: uint8;
    bit_depth_luma_minus8: uint8;
    bit_depth_chroma_minus8: uint8;
    log2_max_frame_num_minus4: uint8;
    pic_order_cntype: StdVideoH264PocType;
    offset_for_non_ref_pic: int32;
    offset_foropo_bottom_field: int32;
    log2_max_pic_order_cnt_lsb_minus4: uint8;
    num_ref_frames_in_pic_order_cnt_cycle: uint8;
    max_num_ref_frames: uint8;
    reserved1: uint8;
    pic_width_in_mbs_minus1: uint32;
    pic_height_in_map_units_minus1: uint32;
    frame_crop_left_offset: uint32;
    frame_crop_right_offset: uint32;
    frame_cropop_offset: uint32;
    frame_crop_bottom_offset: uint32;
    reserved2: uint32;
    pOffsetForRefFrame: Pint32;
    pScalingLists: ^StdVideoH264ScalingLists;
    pSequenceParameterSetVui: ^StdVideoH264SequenceParameterSetVui;
  end;

  StdVideoH264PpsFlags = bitpacked record
    transform_8x8_mode_flag: 0..1;
    redundant_pic_cnt_present_flag: 0..1;
    constrained_intra_pred_flag: 0..1;
    deblocking_filter_control_present_flag: 0..1;
    weighted_pred_flag: 0..1;
    bottom_field_pic_order_in_frame_present_flag: 0..1;
    entropy_coding_mode_flag: 0..1;
    pic_scaling_matrix_present_flag: 0..1;
  end;

  StdVideoH264PictureParameterSet = record
    flags: StdVideoH264PpsFlags;
    seq_parameter_set_id: uint8;
    pic_parameter_set_id: uint8;
    num_ref_idx_l0_default_active_minus1: uint8;
    num_ref_idx_l1_default_active_minus1: uint8;
    weighted_bipred_idc: StdVideoH264WeightedBipredIdc;
    pic_init_qp_minus26: int8;
    pic_init_qs_minus26: int8;
    chroma_qp_index_offset: int8;
    second_chroma_qp_index_offset: int8;
    pScalingLists: ^StdVideoH264ScalingLists;
  end;

const
  VK_STD_VULKAN_VIDEO_CODEC_H264_DECODE_EXTENSION_NAME = 'VK_STD_vulkan_video_codec_h264_decode';
  STD_VIDEO_DECODE_H264_FIELD_ORDER_COUNT_LIST_SIZE = 2;

type
  StdVideoDecodeH264FieldOrderCount = (
    STD_VIDEO_DECODE_H264_FIELD_ORDER_COUNTOP = 0,
    STD_VIDEO_DECODE_H264_FIELD_ORDER_COUNT_BOTTOM = 1,
    STD_VIDEO_DECODE_H264_FIELD_ORDER_COUNT_INVALID = $7FFFFFFF,
    STD_VIDEO_DECODE_H264_FIELD_ORDER_COUNT_MAX_ENUM = $7FFFFFFF);

  StdVideoDecodeH264PictureInfoFlags = bitpacked record
    field_pic_flag: 0..1;
    is_intra: 0..1;
    IdrPicFlag: 0..1;
    bottom_field_flag: 0..1;
    is_reference: 0..1;
    complementary_field_pair: 0..1;
  end;

  StdVideoDecodeH264PictureInfo = record
    flags: StdVideoDecodeH264PictureInfoFlags;
    seq_parameter_set_id: uint8;
    pic_parameter_set_id: uint8;
    reserved1: uint8;
    reserved2: uint8;
    frame_num: uint16;
    idr_pic_id: uint16;
    PicOrderCnt: array[0..STD_VIDEO_DECODE_H264_FIELD_ORDER_COUNT_LIST_SIZE - 1] of int32;
  end;

  StdVideoDecodeH264ReferenceInfoFlags = bitpacked record
    top_field_flag: 0..1;
    bottom_field_flag: 0..1;
    used_for_longerm_reference: 0..1;
    is_non_existing: 0..1;
  end;

  StdVideoDecodeH264ReferenceInfo = record
    flags: StdVideoDecodeH264ReferenceInfoFlags;
    FrameNum: uint16;
    reserved: uint16;
    PicOrderCnt: array [0..STD_VIDEO_DECODE_H264_FIELD_ORDER_COUNT_LIST_SIZE - 1] of int32;
  end;

const
  VK_STD_VULKAN_VIDEO_CODEC_H264_ENCODE_EXTENSION_NAME = 'VK_STD_vulkan_video_codec_h264_encode';

type
  StdVideoEncodeH264WeightTableFlags = record
    luma_weight_l0_flag: uint32;
    chroma_weight_l0_flag: uint32;
    luma_weight_l1_flag: uint32;
    chroma_weight_l1_flag: uint32;
  end;

  PStdVideoEncodeH264WeightTable = ^StdVideoEncodeH264WeightTable;

  StdVideoEncodeH264WeightTable = record
    flags: StdVideoEncodeH264WeightTableFlags;
    luma_log2_weight_denom: uint8;
    chroma_log2_weight_denom: uint8;
    luma_weight_l0: array [0..STD_VIDEO_H264_MAX_NUM_LIST_REF - 1] of int8;
    luma_offset_l0: array [0..STD_VIDEO_H264_MAX_NUM_LIST_REF - 1] of int8;
    chroma_weight_l0: array [0..STD_VIDEO_H264_MAX_NUM_LIST_REF - 1, 0..STD_VIDEO_H264_MAX_CHROMA_PLANES - 1] of int8;
    chroma_offset_l0: array [0..STD_VIDEO_H264_MAX_NUM_LIST_REF - 1, 0..STD_VIDEO_H264_MAX_CHROMA_PLANES - 1] of int8;
    luma_weight_l1: array [0..STD_VIDEO_H264_MAX_NUM_LIST_REF - 1] of int8;
    luma_offset_l1: array [0..STD_VIDEO_H264_MAX_NUM_LIST_REF - 1] of int8;
    chroma_weight_l1: array [0..STD_VIDEO_H264_MAX_NUM_LIST_REF - 1, 0..STD_VIDEO_H264_MAX_CHROMA_PLANES - 1] of int8;
    chroma_offset_l1: array [0..STD_VIDEO_H264_MAX_NUM_LIST_REF - 1, 0..STD_VIDEO_H264_MAX_CHROMA_PLANES - 1] of int8;
  end;

  StdVideoEncodeH264SliceHeaderFlags = record
    direct_spatial_mv_pred_flag: 0..1;
    num_ref_idx_active_override_flag: 0..1;
    reserved: 0..(1 shl 30) - 1;
  end;

  StdVideoEncodeH264PictureInfoFlags = record
    IdrPicFlag: 0..1;
    is_reference: 0..1;
    no_output_of_prior_pics_flag: 0..1;
    longerm_reference_flag: 0..1;
    adaptive_ref_pic_marking_mode_flag: 0..1;
    reserved: 0..27;
  end;

  StdVideoEncodeH264ReferenceInfoFlags = record
    used_for_longerm_reference: 0..1;
    reserved: 0..31;
  end;

  StdVideoEncodeH264ReferenceListsInfoFlags = record
    ref_pic_list_modification_flag_l0: 0..1;
    ref_pic_list_modification_flag_l1: 0..1;
    reserved: 0..(1 shl 30) - 1;
  end;

  PStdVideoEncodeH264RefListModEntry = ^StdVideoEncodeH264RefListModEntry;

  StdVideoEncodeH264RefListModEntry = record
    modification_of_pic_nums_idc: StdVideoH264ModificationOfPicNumsIdc;
    abs_diff_pic_num_minus1: uint16;
    longerm_pic_num: uint16;
  end;

  PStdVideoEncodeH264RefPicMarkingEntry = ^StdVideoEncodeH264RefPicMarkingEntry;

  StdVideoEncodeH264RefPicMarkingEntry = record
    memory_management_control_operation: StdVideoH264MemMgmtControlOp;
    difference_of_pic_nums_minus1: uint16;
    longerm_pic_num: uint16;
    longerm_frame_idx: uint16;
    max_longerm_frame_idx_plus1: uint16;
  end;

  PStdVideoEncodeH264ReferenceListsInfo = ^StdVideoEncodeH264ReferenceListsInfo;

  StdVideoEncodeH264ReferenceListsInfo = record
    flags: StdVideoEncodeH264ReferenceListsInfoFlags;
    num_ref_idx_l0_active_minus1: uint8;
    num_ref_idx_l1_active_minus1: uint8;
    RefPicList0: array[0..STD_VIDEO_H264_MAX_NUM_LIST_REF - 1] of uint8;
    RefPicList1: array[0..STD_VIDEO_H264_MAX_NUM_LIST_REF - 1] of uint8;
    refList0ModOpCount: uint8;
    refList1ModOpCount: uint8;
    refPicMarkingOpCount: uint8;
    reserved1: array[0..7 - 1] of uint8;
    pRefList0ModOperations: PStdVideoEncodeH264RefListModEntry;
    pRefList1ModOperations: PStdVideoEncodeH264RefListModEntry;
    pRefPicMarkingOperations: PStdVideoEncodeH264RefPicMarkingEntry;
  end;

  StdVideoEncodeH264PictureInfo = record
    flags: StdVideoEncodeH264PictureInfoFlags;
    seq_parameter_set_id: uint8;
    pic_parameter_set_id: uint8;
    idr_pic_id: uint16;
    primary_picype: StdVideoH264PictureType;
    frame_num: uint32;
    PicOrderCnt: int32;
    temporal_id: uint8;
    reserved1: array[0..3 - 1] of uint8;
    pRefLists: PStdVideoEncodeH264ReferenceListsInfo;
  end;

  StdVideoEncodeH264ReferenceInfo = record
    flags: StdVideoEncodeH264ReferenceInfoFlags;
    primary_picype: StdVideoH264PictureType;
    FrameNum: uint32;
    PicOrderCnt: int32;
    longerm_pic_num: uint16;
    longerm_frame_idx: uint16;
    temporal_id: uint8;
  end;

  StdVideoEncodeH264SliceHeader = record
    flags: StdVideoEncodeH264SliceHeaderFlags;
    first_mb_in_slice: uint32;
    sliceype: StdVideoH264SliceType;
    slice_alpha_c0_offset_div2: int8;
    slice_beta_offset_div2: int8;
    slice_qp_delta: int8;
    reserved1: uint8;
    cabac_init_idc: StdVideoH264CabacInitIdc;
    disable_deblocking_filter_idc: StdVideoH264DisableDeblockingFilterIdc;
    pWeightTable: PStdVideoEncodeH264WeightTable;
  end;
