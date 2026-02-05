unit vulkan.tutorial.application09;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  vulkan.tutorial,
  vulkan.tutorial.application08,
  pax.vulkan,
  pax.vulkan.helpers;

type
  { TVulkanApplicationTutorial09 }

  TVulkanApplicationTutorial09 = class(TVulkanApplicationTutorial08, IVulkanTutorial)
  protected
    FFramebuffers: array of VkFramebuffer;
  protected
    procedure InitVulkan; override;
    procedure Cleanup; override;
    procedure CreateFramebuffers;
  public
    constructor Create; override;
  end;

implementation

{ TVulkanApplicationTutorial09 }

constructor TVulkanApplicationTutorial09.Create;
begin
  inherited Create;
  Caption := 'Vulkan Tutorial 09 - Framebuffers';
end;

procedure TVulkanApplicationTutorial09.CreateFramebuffers;
var
  i: integer;
  framebufferInfo: VkFramebufferCreateInfo;
  attachments: array[0..0] of VkImageView;
begin
  SetLength(FFramebuffers, Length(FSwapChainImageViews));

  for i := 0 to High(FSwapChainImageViews) do
  begin
    attachments[0] := FSwapChainImageViews[i];
    framebufferInfo.zero;
    framebufferInfo.sType := VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
    framebufferInfo.renderPass := FRenderPass;
    framebufferInfo.attachmentCount := 1;
    framebufferInfo.pAttachments := @attachments[0];
    framebufferInfo.Width := FSwapChainExtent.Width;
    framebufferInfo.Height := FSwapChainExtent.Height;
    framebufferInfo.layers := 1;

    with getVulkan do
      if vkCreateFramebuffer(FDevice, @framebufferInfo, nil, @FFramebuffers[i]) <> VK_SUCCESS then
        raise Exception.Create('Impossibile creare framebuffer');
  end;
end;

procedure TVulkanApplicationTutorial09.InitVulkan;
begin
  inherited InitVulkan;
  CreateFramebuffers;
end;

procedure TVulkanApplicationTutorial09.Cleanup;
var
  i: integer;
begin
  with getVulkan do
    for i := 0 to High(FFramebuffers) do
      vkDestroyFramebuffer(FDevice, FFramebuffers[i], nil);
  inherited Cleanup;
end;

end.
