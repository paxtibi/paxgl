unit vulkan.tutorial.application11;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  vulkan.tutorial.application10,
  vulkan.tutorial,
  pax.vulkan.helpers,
  pax.vulkan,
  pax.glfw;

const
  MAX_FRAMES_IN_FLIGHT = 2;

type
  { TVulkanApplicationTutorial11 }

  TVulkanApplicationTutorial11 = class(TVulkanApplicationTutorial10, IVulkanTutorial)
  protected
    FSwapChainFramebuffers: array of VkFramebuffer;
    FGraphicsPipeline: VkPipeline;
    FPipelineLayout: VkPipelineLayout;
    FCommandPool: VkCommandPool;
    FCommandBuffers: array of VkCommandBuffer;
    FImageAvailableSemaphores: array[0..MAX_FRAMES_IN_FLIGHT - 1] of VkSemaphore;
    FRenderFinishedSemaphores: array[0..MAX_FRAMES_IN_FLIGHT - 1] of VkSemaphore;
    FInFlightFences: array[0..MAX_FRAMES_IN_FLIGHT - 1] of VkFence;
    FCurrentFrame: uint32;
  protected
    procedure InitVulkan; override;
    procedure MainLoop; override;
    procedure Cleanup; override;
    procedure CreateGraphicsPipeline;
    procedure CreateCommandPool;
    procedure CreateCommandBuffers;
    procedure RecordCommandBuffer(commandBuffer: VkCommandBuffer; imageIndex: uint32);
    procedure CreateSyncObjects;
    procedure DrawFrame;
  public
    constructor Create; override;
  end;

implementation

{ TVulkanApplicationTutorial11 }

constructor TVulkanApplicationTutorial11.Create;
begin
  inherited Create;
  FGraphicsPipeline := VK_NULL_HANDLE;
  FPipelineLayout := VK_NULL_HANDLE;
  FCommandPool := VK_NULL_HANDLE;
  FCurrentFrame := 0;
  Caption := 'Vulkan Tutorial 11 - Pipeline + Commands + Rendering';
end;

procedure TVulkanApplicationTutorial11.CreateGraphicsPipeline;
var
  vertCode, fragCode: TBytes;
  vertModule, fragModule: VkShaderModule;
  vertStageInfo, fragStageInfo: VkPipelineShaderStageCreateInfo;
  shaderStages: array[0..1] of VkPipelineShaderStageCreateInfo;
  vertexInputInfo: VkPipelineVertexInputStateCreateInfo;
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

  vertexInputInfo.zero;
  vertexInputInfo.sType := VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
  vertexInputInfo.vertexBindingDescriptionCount := 0;
  vertexInputInfo.vertexAttributeDescriptionCount := 0;

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
  with getVulkan do
  begin
    if vkCreateGraphicsPipelines(FDevice, VK_NULL_HANDLE, 1, @pipelineInfo, nil, @FGraphicsPipeline) <> VK_SUCCESS then
      raise Exception.Create('Impossibile creare graphics pipeline');

    vkDestroyShaderModule(FDevice, fragModule, nil);
    vkDestroyShaderModule(FDevice, vertModule, nil);

  end;
end;

procedure TVulkanApplicationTutorial11.CreateCommandPool;
var
  poolInfo: VkCommandPoolCreateInfo;
begin
  poolInfo.zero;
  poolInfo.sType := VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
  poolInfo.flags := VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;
  poolInfo.queueFamilyIndex := findQueueFamilies(FPhysicalDevice).graphicsFamily;

  with getVulkan do
    if vkCreateCommandPool(FDevice, @poolInfo, nil, @FCommandPool) <> VK_SUCCESS then
      raise Exception.Create('Impossibile creare command pool');
end;

procedure TVulkanApplicationTutorial11.CreateCommandBuffers;
var
  allocInfo: VkCommandBufferAllocateInfo;
  i: integer;
begin

  SetLength(FCommandBuffers, Length(FSwapChainFramebuffers));

  allocInfo.zero;
  allocInfo.sType := VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
  allocInfo.commandPool := FCommandPool;
  allocInfo.level := VK_COMMAND_BUFFER_LEVEL_PRIMARY;
  allocInfo.commandBufferCount := Length(FCommandBuffers);

  with getVulkan do
    if vkAllocateCommandBuffers(FDevice, @allocInfo, @FCommandBuffers[0]) <> VK_SUCCESS then
      raise Exception.Create('Impossibile allocare command buffers');

  for i := 0 to High(FCommandBuffers) do
    RecordCommandBuffer(FCommandBuffers[i], i);
end;

procedure TVulkanApplicationTutorial11.RecordCommandBuffer(commandBuffer: VkCommandBuffer; imageIndex: uint32);
var
  beginInfo: VkCommandBufferBeginInfo;
  renderPassInfo: VkRenderPassBeginInfo;
  clearColor: VkClearValue;
  viewport: VkViewport;
  scissor: VkRect2D;
begin
  beginInfo.zero;
  beginInfo.sType := VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;

  with getVulkan do
    if vkBeginCommandBuffer(commandBuffer, @beginInfo) <> VK_SUCCESS then
      raise Exception.Create('Impossibile iniziare command buffer');

  renderPassInfo.zero;
  renderPassInfo.sType := VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
  renderPassInfo.renderPass := FRenderPass;
  renderPassInfo.framebuffer := FFramebuffers[imageIndex];
  renderPassInfo.renderArea.offset.x := 0;
  renderPassInfo.renderArea.offset.y := 0;
  renderPassInfo.renderArea.extent := FSwapChainExtent;

  clearColor.color.float32[0] := 0.0;
  clearColor.color.float32[1] := 0.0;
  clearColor.color.float32[2] := 0.0;
  clearColor.color.float32[3] := 1.0;
  renderPassInfo.clearValueCount := 1;
  renderPassInfo.pClearValues := @clearColor;
  with getVulkan do
  begin
    vkCmdBeginRenderPass(commandBuffer, @renderPassInfo, VK_SUBPASS_CONTENTS_INLINE);

    vkCmdBindPipeline(commandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, FGraphicsPipeline);

    viewport.x := 0.0;
    viewport.y := 0.0;
    viewport.Width := FSwapChainExtent.Width;
    viewport.Height := FSwapChainExtent.Height;
    viewport.minDepth := 0.0;
    viewport.maxDepth := 1.0;
    vkCmdSetViewport(commandBuffer, 0, 1, @viewport);

    scissor.offset.x := 0;
    scissor.offset.y := 0;
    scissor.extent := FSwapChainExtent;
    vkCmdSetScissor(commandBuffer, 0, 1, @scissor);

    vkCmdDraw(commandBuffer, 3, 1, 0, 0);

    vkCmdEndRenderPass(commandBuffer);

    if vkEndCommandBuffer(commandBuffer) <> VK_SUCCESS then
      raise Exception.Create('Impossibile registrare command buffer');
  end;
end;

procedure TVulkanApplicationTutorial11.CreateSyncObjects;
var
  semaphoreInfo: VkSemaphoreCreateInfo;
  fenceInfo: VkFenceCreateInfo;
  i: integer;
begin
  semaphoreInfo.zero;
  semaphoreInfo.sType := VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;

  fenceInfo.zero;
  fenceInfo.sType := VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
  fenceInfo.flags := VK_FENCE_CREATE_SIGNALED_BIT;

  with getVulkan do
    for i := 0 to MAX_FRAMES_IN_FLIGHT - 1 do
    begin
      vkCreateSemaphore(FDevice, @semaphoreInfo, nil, @FImageAvailableSemaphores[i]);
      vkCreateSemaphore(FDevice, @semaphoreInfo, nil, @FRenderFinishedSemaphores[i]);
      vkCreateFence(FDevice, @fenceInfo, nil, @FInFlightFences[i]);
    end;
end;

procedure TVulkanApplicationTutorial11.DrawFrame;
var
  imageIndex: uint32;
  waitSemaphores: array[0..0] of VkSemaphore;
  waitStages: array[0..0] of VkPipelineStageFlags;
  signalSemaphores: array[0..0] of VkSemaphore;
  submitInfo: VkSubmitInfo;
  presentInfo: VkPresentInfoKHR;
begin
  with getVulkan do
  begin
    vkWaitForFences(FDevice, 1, @FInFlightFences[FCurrentFrame], VK_TRUE, High(uint64));
    vkResetFences(FDevice, 1, @FInFlightFences[FCurrentFrame]);

    FSwapChainExtension.vkAcquireNextImageKHR(FSwapChainHandle, High(uint64), FImageAvailableSemaphores[FCurrentFrame], VK_NULL_HANDLE, @imageIndex);

    waitSemaphores[0] := FImageAvailableSemaphores[FCurrentFrame];
    waitStages[0] := VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
    signalSemaphores[0] := FRenderFinishedSemaphores[FCurrentFrame];

    submitInfo.zero;
    submitInfo.sType := VK_STRUCTURE_TYPE_SUBMIT_INFO;
    submitInfo.waitSemaphoreCount := 1;
    submitInfo.pWaitSemaphores := @waitSemaphores[0];
    submitInfo.pWaitDstStageMask := @waitStages[0];
    submitInfo.commandBufferCount := 1;
    submitInfo.pCommandBuffers := @FCommandBuffers[imageIndex];
    submitInfo.signalSemaphoreCount := 1;
    submitInfo.pSignalSemaphores := @signalSemaphores[0];

    vkQueueSubmit(FGraphicsQueue, 1, @submitInfo, FInFlightFences[FCurrentFrame]);

    presentInfo.zero;
    presentInfo.sType := VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
    presentInfo.waitSemaphoreCount := 1;
    presentInfo.pWaitSemaphores := @signalSemaphores[0];
    presentInfo.swapchainCount := 1;
    presentInfo.pSwapchains := @FSwapChainHandle;
    presentInfo.pImageIndices := @imageIndex;

    FSwapChainExtension.vkQueuePresentKHR(FPresentQueue, @presentInfo);

    FCurrentFrame := (FCurrentFrame + 1) mod MAX_FRAMES_IN_FLIGHT;
  end;
end;

procedure TVulkanApplicationTutorial11.InitVulkan;
begin
  inherited InitVulkan;
  CreateGraphicsPipeline;
  CreateCommandPool;
  CreateCommandBuffers;
  CreateSyncObjects;
end;

procedure TVulkanApplicationTutorial11.MainLoop;
begin
  inherited MainLoop;
  with getGLFW do
    while not glfwWindowShouldClose(FWindow) do
    begin
      glfwPollEvents;
      DrawFrame;
    end;
end;

procedure TVulkanApplicationTutorial11.Cleanup;
var
  i: integer;
begin
  with getVulkan do
  begin
    vkDeviceWaitIdle(FDevice);
    for i := 0 to MAX_FRAMES_IN_FLIGHT - 1 do
    begin
      vkDestroySemaphore(FDevice, FImageAvailableSemaphores[i], nil);
      vkDestroySemaphore(FDevice, FRenderFinishedSemaphores[i], nil);
      vkDestroyFence(FDevice, FInFlightFences[i], nil);
    end;
    vkFreeCommandBuffers(FDevice, FCommandPool, Length(FCommandBuffers), @FCommandBuffers[0]);
    vkDestroyCommandPool(FDevice, FCommandPool, nil);
    vkDestroyPipeline(FDevice, FGraphicsPipeline, nil);
    vkDestroyPipelineLayout(FDevice, FPipelineLayout, nil);
  end;
  inherited Cleanup;
end;

end.
