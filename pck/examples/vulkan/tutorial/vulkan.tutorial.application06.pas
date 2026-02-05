unit vulkan.tutorial.application06;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  vulkan.tutorial,
  vulkan.tutorial.application05,
  pax.glfw,
  pax.vulkan,
  pax.vulkan.helpers;

const
  DEVICE_EXTENSION_SWAPCHAIN = VK_KHR_SWAPCHAIN_EXTENSION_NAME;

type
  TSwapChainSupportDetails = record
    Capabilities: VkSurfaceCapabilitiesKHR;
    Formats: array of VkSurfaceFormatKHR;
    PresentModes: array of VkPresentModeKHR;
  end;

  { TVulkanApplicationTutorial06 }

  TVulkanApplicationTutorial06 = class(TVulkanApplicationTutorial05)
  protected
    FSwapChainExtension: IVulkanSwapChainExtension;
    FSwapChainHandle: VkSwapchainKHR;
    FSwapChainImages: array of VkImage;
    FSwapChainImageFormat: VkFormat;
    FSwapChainExtent: VkExtent2D;
  protected
    function ensureSwapChainExtension: IVulkanSwapChainExtension; inline;
    function ensureSurface: IVulkanKHRSurface; inline;
    procedure InitVulkan; override;
    procedure Cleanup; override;
    procedure CreateSwapChain;
    function QuerySwapChainSupport(PhysicalDevice: VkPhysicalDevice): TSwapChainSupportDetails;
    function ChooseSwapSurfaceFormat(const AvailableFormats: array of VkSurfaceFormatKHR): VkSurfaceFormatKHR;
    function ChooseSwapPresentMode(const AvailablePresentModes: array of VkPresentModeKHR): VkPresentModeKHR;
    function ChooseSwapExtent(const Capabilities: VkSurfaceCapabilitiesKHR): VkExtent2D;
    function CheckDeviceExtensionSupport(Device: VkPhysicalDevice): boolean;
  public
    constructor Create; override;
  end;

implementation

{ TVulkanApplicationTutorial06 }

constructor TVulkanApplicationTutorial06.Create;
begin
  getLogger.Enter('TVulkanApplicationTutorial06', 'Create');
  inherited Create;
  FSwapChainHandle := VK_NULL_HANDLE;
  Caption := 'Vulkan Tutorial 06 - Swap Chain';
  FDeviceExtensions.Add(DEVICE_EXTENSION_SWAPCHAIN);
  getLogger.Leave('TVulkanApplicationTutorial06', 'Create');
end;

function TVulkanApplicationTutorial06.CheckDeviceExtensionSupport(Device: VkPhysicalDevice): boolean;
var
  extCount: uint32;
  extensions: array of VkExtensionProperties;
  i: integer;
begin
  getLogger.Enter('TVulkanApplicationTutorial06', 'CheckDeviceExtensionSupport');
  with getVulkan do
  begin
    vkEnumerateDeviceExtensionProperties(Device, nil, @extCount, nil);
    SetLength(extensions, extCount);
    vkEnumerateDeviceExtensionProperties(Device, nil, @extCount, @extensions[0]);
    for i := 0 to High(extensions) do
      if StrPas(extensions[i].extensionName) = DEVICE_EXTENSION_SWAPCHAIN then
        Exit(True);
  end;
  Result := False;
  getLogger.Leave('TVulkanApplicationTutorial06', 'CheckDeviceExtensionSupport');
end;

function TVulkanApplicationTutorial06.QuerySwapChainSupport(PhysicalDevice: VkPhysicalDevice): TSwapChainSupportDetails;
var
  details: TSwapChainSupportDetails;
  formatCount, presentModeCount: uint32;
begin
  getLogger.Enter('TVulkanApplicationTutorial06', 'QuerySwapChainSupport');
  with getVulkan do
  begin
    ensureSurface.vkGetPhysicalDeviceSurfaceCapabilitiesKHR(PhysicalDevice, FSurface, @details.Capabilities);

    ensureSurface.vkGetPhysicalDeviceSurfaceFormatsKHR(PhysicalDevice, FSurface, @formatCount, nil);
    SetLength(details.Formats, formatCount);
    ensureSurface.vkGetPhysicalDeviceSurfaceFormatsKHR(PhysicalDevice, FSurface, @formatCount, @details.Formats[0]);

    ensureSurface.vkGetPhysicalDeviceSurfacePresentModesKHR(PhysicalDevice, FSurface, @presentModeCount, nil);
    SetLength(details.PresentModes, presentModeCount);
    ensureSurface.vkGetPhysicalDeviceSurfacePresentModesKHR(PhysicalDevice, FSurface, @presentModeCount, @details.PresentModes[0]);
  end;
  Result := details;
  getLogger.Leave('TVulkanApplicationTutorial06', 'QuerySwapChainSupport');
end;

function TVulkanApplicationTutorial06.ChooseSwapSurfaceFormat(const AvailableFormats: array of VkSurfaceFormatKHR): VkSurfaceFormatKHR;
var
  fmt: VkSurfaceFormatKHR;
begin
  getLogger.Enter('TVulkanApplicationTutorial06', 'ChooseSwapSurfaceFormat');
  for fmt in AvailableFormats do
    if (fmt.format = VK_FORMAT_B8G8R8A8_SRGB) and (fmt.colorSpace = VK_COLOR_SPACE_SRGB_NONLINEAR_KHR) then
      Exit(fmt);
  Result := AvailableFormats[0];
  getLogger.Leave('TVulkanApplicationTutorial06', 'ChooseSwapSurfaceFormat');
end;

function TVulkanApplicationTutorial06.ChooseSwapPresentMode(const AvailablePresentModes: array of VkPresentModeKHR): VkPresentModeKHR;
var
  mode: VkPresentModeKHR;
begin
  getLogger.Enter('TVulkanApplicationTutorial06', 'ChooseSwapPresentMode');
  for mode in AvailablePresentModes do
    if mode = VK_PRESENT_MODE_MAILBOX_KHR then
    begin
      Exit(mode);
      getLogger.Leave('TVulkanApplicationTutorial06', 'ChooseSwapPresentMode');
    end;
  Result := VK_PRESENT_MODE_FIFO_KHR;
  getLogger.Leave('TVulkanApplicationTutorial06', 'ChooseSwapPresentMode');
end;

function TVulkanApplicationTutorial06.ChooseSwapExtent(const Capabilities: VkSurfaceCapabilitiesKHR): VkExtent2D;
var
  Width: integer = 0;
  Height: integer = 0;
begin
  getLogger.Enter('TVulkanApplicationTutorial06', 'ChooseSwapExtent');
  if Capabilities.currentExtent.Width <> High(uint32) then
  begin
    getLogger.Leave('TVulkanApplicationTutorial06', 'ChooseSwapExtent');
    Exit(Capabilities.currentExtent);
  end
  else
  begin
    getGLFW.glfwGetFramebufferSize(FWindow, Width, Height);
    Result.Width := Width;
    Result.Height := Height;
    if Result.Width < Capabilities.minImageExtent.Width then Result.Width := Capabilities.minImageExtent.Width;
    if Result.Width > Capabilities.maxImageExtent.Width then Result.Width := Capabilities.maxImageExtent.Width;
    if Result.Height < Capabilities.minImageExtent.Height then Result.Height := Capabilities.minImageExtent.Height;
    if Result.Height > Capabilities.maxImageExtent.Height then Result.Height := Capabilities.maxImageExtent.Height;
  end;
  getLogger.Leave('TVulkanApplicationTutorial06', 'ChooseSwapExtent');
end;

procedure TVulkanApplicationTutorial06.CreateSwapChain;
var
  details: TSwapChainSupportDetails;
  surfaceFormat: VkSurfaceFormatKHR;
  presentMode: VkPresentModeKHR;
  extent: VkExtent2D;
  imageCount: uint32;
  createInfo: VkSwapchainCreateInfoKHR;
  queueIndices: array[0..1] of uint32;
begin
  getLogger.Enter('TVulkanApplicationTutorial06', 'CreateSwapChain');
  details := QuerySwapChainSupport(FPhysicalDevice);
  surfaceFormat := ChooseSwapSurfaceFormat(details.Formats);
  presentMode := ChooseSwapPresentMode(details.PresentModes);
  extent := ChooseSwapExtent(details.Capabilities);

  imageCount := details.Capabilities.minImageCount + 1;
  if (details.Capabilities.maxImageCount > 0) and (imageCount > details.Capabilities.maxImageCount) then
    imageCount := details.Capabilities.maxImageCount;

  createInfo.zero;
  createInfo.sType := VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
  createInfo.surface := FSurface;
  createInfo.minImageCount := imageCount;
  createInfo.imageFormat := surfaceFormat.format;
  createInfo.imageColorSpace := surfaceFormat.colorSpace;
  createInfo.imageExtent := extent;
  createInfo.imageArrayLayers := 1;
  createInfo.imageUsage := VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;

  queueIndices[0] := findQueueFamilies(FPhysicalDevice).graphicsFamily;
  queueIndices[1] := findQueueFamilies(FPhysicalDevice).presentFamily;  // Assumendo presentFamily aggiunto in tutorial 05

  if queueIndices[0] <> queueIndices[1] then
  begin
    createInfo.imageSharingMode := VK_SHARING_MODE_CONCURRENT;
    createInfo.queueFamilyIndexCount := 2;
    createInfo.pQueueFamilyIndices := @queueIndices[0];
  end
  else
  begin
    createInfo.imageSharingMode := VK_SHARING_MODE_EXCLUSIVE;
  end;

  createInfo.preTransform := details.Capabilities.currentTransform;
  createInfo.compositeAlpha := VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
  createInfo.presentMode := presentMode;
  createInfo.clipped := VK_TRUE;

  FSwapChainExtension := ensureSwapChainExtension();
  if FSwapChainExtension.vkCreateSwapchainKHR(@createInfo, nil, @FSwapChainHandle) <> VK_SUCCESS then
    raise Exception.Create('Impossibile creare swap chain');

  FSwapChainExtension.vkGetSwapchainImagesKHR(FSwapChainHandle, @imageCount, nil);
  SetLength(FSwapChainImages, imageCount);
  FSwapChainExtension.vkGetSwapchainImagesKHR(FSwapChainHandle, @imageCount, @FSwapChainImages[0]);

  FSwapChainImageFormat := surfaceFormat.format;
  FSwapChainExtent := extent;
  getLogger.Leave('TVulkanApplicationTutorial06', 'CreateSwapChain');
end;

function TVulkanApplicationTutorial06.ensureSwapChainExtension: IVulkanSwapChainExtension;
begin
  getLogger.Enter('TVulkanApplicationTutorial06', 'ensureSwapChainExtension');
  if FSwapChainExtension = nil then
  try
    FSwapChainExtension := getVulkan.getSwapChainExtension(FDevice);
  except
    on e: Exception do
    begin
      getLogger.Leave('TVulkanApplicationTutorial06', 'ensureSwapChainExtension with ' + e.Message);
      raise;
    end;
  end;
  Result := FSwapChainExtension;
  getLogger.Leave('TVulkanApplicationTutorial06', 'ensureSwapChainExtension');
end;

function TVulkanApplicationTutorial06.ensureSurface: IVulkanKHRSurface;
begin
  getLogger.Enter('TVulkanApplicationTutorial06', 'ensureSurface');
  if FSurfaceExtension = nil then
    FSurfaceExtension := getVulkan.getKHRSurfaceExtension(FInstance);
  Result := FSurfaceExtension;
  getLogger.Leave('TVulkanApplicationTutorial06', 'ensureSurface');
end;

procedure TVulkanApplicationTutorial06.InitVulkan;
begin
  getLogger.Enter('TVulkanApplicationTutorial06', 'InitVulkan');
  inherited InitVulkan;
  CreateSwapChain;
  getLogger.Leave('TVulkanApplicationTutorial06', 'InitVulkan');
end;

procedure TVulkanApplicationTutorial06.Cleanup;
begin
  getLogger.Enter('TVulkanApplicationTutorial06', 'Cleanup');
  if FSwapChainHandle <> VK_NULL_HANDLE then
    FSwapChainExtension.vkDestroySwapchainKHR(FSwapChainHandle, nil);
  inherited Cleanup;
  getLogger.Leave('TVulkanApplicationTutorial06', 'Cleanup');
end;

end.
