unit vulkan.tutorial.application08;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  vulkan.tutorial,
  vulkan.tutorial.application07,
  pax.vulkan,
  pax.vulkan.helpers;

type
  { TVulkanApplicationTutorial08 }

  TVulkanApplicationTutorial08 = class(TVulkanApplicationTutorial07, IVulkanTutorial)
  protected
    FRenderPass: VkRenderPass;
  protected
    procedure InitVulkan; override;
    procedure Cleanup; override;
    procedure CreateRenderPass;
  public
    constructor Create; override;
  end;

implementation

{ TVulkanApplicationTutorial08 }

constructor TVulkanApplicationTutorial08.Create;
begin
  inherited Create;
  FRenderPass := VK_NULL_HANDLE;
  Caption := 'Vulkan Tutorial 08 - Render Passes';
end;

procedure TVulkanApplicationTutorial08.CreateRenderPass;
var
  colorAttachment: VkAttachmentDescription;
  colorAttachmentRef: VkAttachmentReference;
  subpass: VkSubpassDescription;
  renderPassInfo: VkRenderPassCreateInfo;
begin
  colorAttachment.zero;
  colorAttachment.format := FSwapChainImageFormat;
  colorAttachment.samples := VK_SAMPLE_COUNT_1_BIT;
  colorAttachment.loadOp := VK_ATTACHMENT_LOAD_OP_CLEAR;
  colorAttachment.storeOp := VK_ATTACHMENT_STORE_OP_STORE;
  colorAttachment.stencilLoadOp := VK_ATTACHMENT_LOAD_OP_DONT_CARE;
  colorAttachment.stencilStoreOp := VK_ATTACHMENT_STORE_OP_DONT_CARE;
  colorAttachment.initialLayout := VK_IMAGE_LAYOUT_UNDEFINED;
  colorAttachment.finalLayout := VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;

  colorAttachmentRef.attachment := 0;
  colorAttachmentRef.layout := VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;

  subpass.zero;
  subpass.pipelineBindPoint := VK_PIPELINE_BIND_POINT_GRAPHICS;
  subpass.colorAttachmentCount := 1;
  subpass.pColorAttachments := @colorAttachmentRef;

  renderPassInfo.zero;
  renderPassInfo.sType := VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
  renderPassInfo.attachmentCount := 1;
  renderPassInfo.pAttachments := @colorAttachment;
  renderPassInfo.subpassCount := 1;
  renderPassInfo.pSubpasses := @subpass;

  with getVulkan do
    if vkCreateRenderPass(FDevice, @renderPassInfo, nil, @FRenderPass) <> VK_SUCCESS then
      raise Exception.Create('Impossibile creare render pass');
end;

procedure TVulkanApplicationTutorial08.InitVulkan;
begin
  inherited InitVulkan;
  CreateRenderPass;
end;

procedure TVulkanApplicationTutorial08.Cleanup;
begin
  with getVulkan do
    if FRenderPass <> VK_NULL_HANDLE then
      vkDestroyRenderPass(FDevice, FRenderPass, nil);
  inherited Cleanup;
end;

end.
