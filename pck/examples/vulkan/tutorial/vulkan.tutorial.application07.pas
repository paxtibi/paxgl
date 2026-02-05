unit vulkan.tutorial.application07;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  vulkan.tutorial,
  vulkan.tutorial.application06,
  pax.vulkan,
  pax.vulkan.helpers;

type
  { TVulkanApplicationTutorial07 }

  TVulkanApplicationTutorial07 = class(TVulkanApplicationTutorial06)
  protected
    FSwapChainImageViews: array of VkImageView;
  protected
    procedure InitVulkan; override;
    procedure Cleanup; override;
    procedure CreateImageViews;
  public
    constructor Create; override;
  end;

implementation

{ TVulkanApplicationTutorial07 }

constructor TVulkanApplicationTutorial07.Create;
begin
  getLogger.Enter('TVulkanApplicationTutorial07', 'Create');
  inherited Create;
  Caption := 'Vulkan Tutorial 07 - Image Views';
  getLogger.Leave('TVulkanApplicationTutorial07', 'Create');
end;

procedure TVulkanApplicationTutorial07.CreateImageViews;
var
  i: integer;
  createInfo: VkImageViewCreateInfo;
begin
  getLogger.Enter('TVulkanApplicationTutorial07', 'CreateImageViews');

  SetLength(FSwapChainImageViews, Length(FSwapChainImages));

  for i := 0 to High(FSwapChainImages) do
  begin
    createInfo.zero;
    createInfo.sType := VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
    createInfo.image := FSwapChainImages[i];
    createInfo.viewType := VK_IMAGE_VIEW_TYPE_2D;
    createInfo.format := FSwapChainImageFormat;
    createInfo.Components.r := VK_COMPONENT_SWIZZLE_IDENTITY;
    createInfo.Components.g := VK_COMPONENT_SWIZZLE_IDENTITY;
    createInfo.Components.b := VK_COMPONENT_SWIZZLE_IDENTITY;
    createInfo.Components.a := VK_COMPONENT_SWIZZLE_IDENTITY;
    createInfo.subresourceRange.aspectMask := VK_IMAGE_ASPECT_COLOR_BIT;
    createInfo.subresourceRange.baseMipLevel := 0;
    createInfo.subresourceRange.levelCount := 1;
    createInfo.subresourceRange.baseArrayLayer := 0;
    createInfo.subresourceRange.layerCount := 1;

    with getVulkan do
      if vkCreateImageView(FDevice, @createInfo, nil, @FSwapChainImageViews[i]) <> VK_SUCCESS then
        raise Exception.Create('Impossibile creare image view');
  end;
  getLogger.Leave('TVulkanApplicationTutorial07', 'Cleanup');
end;

procedure TVulkanApplicationTutorial07.InitVulkan;
begin
  getLogger.Enter('TVulkanApplicationTutorial07', 'InitVulkan');
  inherited InitVulkan;
  CreateImageViews;
  getLogger.Leave('TVulkanApplicationTutorial07', 'InitVulkan');
end;

procedure TVulkanApplicationTutorial07.Cleanup;
var
  i: integer;
begin
  getLogger.Enter('TVulkanApplicationTutorial07', 'Cleanup');
  with getVulkan do
    for i := 0 to High(FSwapChainImageViews) do
      vkDestroyImageView(FDevice, FSwapChainImageViews[i], nil);
  inherited Cleanup;
  getLogger.Leave('TVulkanApplicationTutorial07', 'Cleanup');
end;

end.
