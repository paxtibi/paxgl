unit vulkan.tutorial.application13;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  pax.vulkan,
  pax.vulkan.helpers,
  vulkan.tutorial,
  vulkan.tutorial.application12;

type



  VkVertexInputAttributeDescriptionArray = array of VkVertexInputAttributeDescription;

  { TVulkanApplicationTutorial13 }

  TVulkanApplicationTutorial13 = class(TVulkanApplicationTutorial12, IVulkanTutorial)
  protected
    procedure CreateGraphicsPipeline;
    function GetBindingDescription: VkVertexInputBindingDescription;
    function GetAttributeDescriptions: VkVertexInputAttributeDescriptionArray;
  public
    constructor Create; override;
  end;

implementation

{ TVulkanApplicationTutorial13 }

constructor TVulkanApplicationTutorial13.Create;
begin
  inherited Create;
  Caption := 'Vulkan Tutorial 13 - Vertex Input Description';
end;

function TVulkanApplicationTutorial13.GetBindingDescription: VkVertexInputBindingDescription;
begin
  Result.binding := 0;
  Result.stride := SizeOf(TVertex);
  Result.inputRate := VK_VERTEX_INPUT_RATE_VERTEX;
end;

function TVulkanApplicationTutorial13.GetAttributeDescriptions: VkVertexInputAttributeDescriptionArray;
begin
  SetLength(Result, 2);
  Result[0].binding := 0;
  Result[0].location := 0;
  Result[0].format := VK_FORMAT_R32G32_SFLOAT;
  Result[0].offset := 0;  // Offset di pos

  Result[1].binding := 0;
  Result[1].location := 1;
  Result[1].format := VK_FORMAT_R32G32B32_SFLOAT;
  Result[1].offset := SizeOf(TVec2);  // Offset di color
end;

procedure TVulkanApplicationTutorial13.CreateGraphicsPipeline;
var
  vertCode, fragCode: TBytes;
  vertModule, fragModule: VkShaderModule;
  vertStageInfo, fragStageInfo: VkPipelineShaderStageCreateInfo;
  shaderStages: array[0..1] of VkPipelineShaderStageCreateInfo;
  vertexInputInfo: VkPipelineVertexInputStateCreateInfo;
  bindingDescription: VkVertexInputBindingDescription;
  attributeDescriptions: array of VkVertexInputAttributeDescription;
  inputAssembly: VkPipelineInputAssemblyStateCreateInfo;
  viewport: VkViewport;
  scissor: VkRect2D;
  viewportState: VkPipelineViewportStateCreateInfo;
  rasterizer: VkPipelineRasterizationStateCreateInfo;
  multisampling: VkPipelineMultisampleStateCreateInfo;
  colorBlendAttachment: VkPipelineColorBlendAttachmentState;
  colorBlending: VkPipelineColorBlendStateCreateInfo;
  dynamicStates: array[0..1] of VkDynamicState;
  dynamicState: VkPipelineDynamicStateCreateInfo;
  pipelineLayoutInfo: VkPipelineLayoutCreateInfo;
  pipelineInfo: VkGraphicsPipelineCreateInfo;
begin
  vertCode := ReadFileToBytes('shaders/vert.spv');
  fragCode := ReadFileToBytes('shaders/frag.spv');
  vertModule := CreateShaderModule(vertCode);
  fragModule := CreateShaderModule(fragCode);

  vertStageInfo.zero;
  vertStageInfo.sType := VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
  vertStageInfo.stage := VK_SHADER_STAGE_VERTEX_BIT;
  vertStageInfo.module := vertModule;
  vertStageInfo.pName := 'main';

  fragStageInfo.zero;
  fragStageInfo.sType := VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
  fragStageInfo.stage := VK_SHADER_STAGE_FRAGMENT_BIT;
  fragStageInfo.module := fragModule;
  fragStageInfo.pName := 'main';

  shaderStages[0] := vertStageInfo;
  shaderStages[1] := fragStageInfo;

  bindingDescription := GetBindingDescription;
  attributeDescriptions := GetAttributeDescriptions;

  vertexInputInfo.zero;
  vertexInputInfo.sType := VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
  vertexInputInfo.vertexBindingDescriptionCount := 1;
  vertexInputInfo.pVertexBindingDescriptions := @bindingDescription;
  vertexInputInfo.vertexAttributeDescriptionCount := Length(attributeDescriptions);
  vertexInputInfo.pVertexAttributeDescriptions := @attributeDescriptions[0];

  inputAssembly.zero;
  inputAssembly.sType := VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
  inputAssembly.topology := VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
  inputAssembly.primitiveRestartEnable := VK_FALSE;

  viewport.x := 0.0;
  viewport.y := 0.0;
  viewport.Width := FSwapChainExtent.Width;
  viewport.Height := FSwapChainExtent.Height;
  viewport.minDepth := 0.0;
  viewport.maxDepth := 1.0;

  scissor.offset.x := 0;
  scissor.offset.y := 0;
  scissor.extent := FSwapChainExtent;

  viewportState.zero;
  viewportState.sType := VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
  viewportState.viewportCount := 1;
  viewportState.pViewports := @viewport;
  viewportState.scissorCount := 1;
  viewportState.pScissors := @scissor;

  rasterizer.zero;
  rasterizer.sType := VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
  rasterizer.depthClampEnable := VK_FALSE;
  rasterizer.rasterizerDiscardEnable := VK_FALSE;
  rasterizer.polygonMode := VK_POLYGON_MODE_FILL;
  rasterizer.lineWidth := 1.0;
  rasterizer.cullMode := VK_CULL_MODE_BACK_BIT;
  rasterizer.frontFace := VK_FRONT_FACE_CLOCKWISE;
  rasterizer.depthBiasEnable := VK_FALSE;

  multisampling.zero;
  multisampling.sType := VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
  multisampling.sampleShadingEnable := VK_FALSE;
  multisampling.rasterizationSamples := VK_SAMPLE_COUNT_1_BIT;

  colorBlendAttachment.zero;
  colorBlendAttachment.colorWriteMask := VK_COLOR_COMPONENT_R_BIT or VK_COLOR_COMPONENT_G_BIT or VK_COLOR_COMPONENT_B_BIT or VK_COLOR_COMPONENT_A_BIT;
  colorBlendAttachment.blendEnable := VK_FALSE;

  colorBlending.zero;
  colorBlending.sType := VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO;
  colorBlending.logicOpEnable := VK_FALSE;
  colorBlending.attachmentCount := 1;
  colorBlending.pAttachments := @colorBlendAttachment;

  dynamicStates[0] := VK_DYNAMIC_STATE_VIEWPORT;
  dynamicStates[1] := VK_DYNAMIC_STATE_SCISSOR;

  dynamicState.zero;
  dynamicState.sType := VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO;
  dynamicState.dynamicStateCount := 2;
  dynamicState.pDynamicStates := @dynamicStates[0];

  pipelineLayoutInfo.zero;
  pipelineLayoutInfo.sType := VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;

  with getVulkan do
  begin
    if vkCreatePipelineLayout(FDevice, @pipelineLayoutInfo, nil, @FPipelineLayout) <> VK_SUCCESS then
      raise Exception.Create('Impossibile creare pipeline layout');

    pipelineInfo.zero;
    pipelineInfo.sType := VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO;
    pipelineInfo.stageCount := 2;
    pipelineInfo.pStages := @shaderStages[0];
    pipelineInfo.pVertexInputState := @vertexInputInfo;
    pipelineInfo.pInputAssemblyState := @inputAssembly;
    pipelineInfo.pViewportState := @viewportState;
    pipelineInfo.pRasterizationState := @rasterizer;
    pipelineInfo.pMultisampleState := @multisampling;
    pipelineInfo.pColorBlendState := @colorBlending;
    pipelineInfo.pDynamicState := @dynamicState;
    pipelineInfo.layout := FPipelineLayout;
    pipelineInfo.renderPass := FRenderPass;
    pipelineInfo.subpass := 0;

    if vkCreateGraphicsPipelines(FDevice, VK_NULL_HANDLE, 1, @pipelineInfo, nil, @FGraphicsPipeline) <> VK_SUCCESS then
      raise Exception.Create('Impossibile creare graphics pipeline');

    vkDestroyShaderModule(FDevice, fragModule, nil);
    vkDestroyShaderModule(FDevice, vertModule, nil);
  end;
end;

end.
