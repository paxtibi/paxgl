unit vulkan.tutorial.application19;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  pax.vulkan,
  pax.vulkan.helpers,
  vulkan.tutorial,
  vulkan.tutorial.application17,
  vulkan.tutorial.application18;

type
  { TVulkanApplicationTutorial19 }

  TVulkanApplicationTutorial19 = class(TVulkanApplicationTutorial18, IVulkanTutorial)
  protected
    FTextureImage: VkImage;
    FTextureImageMemory: VkDeviceMemory;
    FTextureImageView: VkImageView;
    FTextureSampler: VkSampler;
  protected
    procedure InitVulkan; override;
    procedure Cleanup; override;
    procedure CreateTextureImage;
    procedure CreateImage(Width, Height: uint32; format: VkFormat; tiling: VkImageTiling; usage: VkImageUsageFlags; properties: VkMemoryPropertyFlags; out image: VkImage; out imageMemory: VkDeviceMemory);
    procedure TransitionImageLayout(image: VkImage; format: VkFormat; oldLayout, newLayout: VkImageLayout);
    procedure CopyBufferToImage(buffer: VkBuffer; image: VkImage; Width, Height: uint32);
    procedure CreateTextureImageView;
    procedure CreateTextureSampler;
    procedure CreateDescriptorSetLayout;
    procedure CreateDescriptorPool;
    procedure CreateDescriptorSets;
    function CreateImageView(image: VkImage; format: VkFormat; aspectFlags: VkImageAspectFlags): VkImageView;
    function LoadImageFromFile(const filename: string; out Width, Height: integer; out pixels: Pointer): VkDeviceSize;
  public
    constructor Create; override;
  end;

implementation

{ TVulkanApplicationTutorial19 }

constructor TVulkanApplicationTutorial19.Create;
begin
  inherited Create;
  Caption := 'Vulkan Tutorial 19 - Texture Mapping';
end;

function TVulkanApplicationTutorial19.LoadImageFromFile(const filename: string; out Width, Height: integer; out pixels: Pointer): VkDeviceSize;
begin
  // Implementa con libreria (es. stbi_load o FreeImage)
  // Esempio placeholder:
  Width := 1024;
  Height := 1024;
  GetMem(pixels, Width * Height * 4);
  Result := Width * Height * 4;
  // Carica realmente i pixel qui
end;

procedure TVulkanApplicationTutorial19.CreateImage(Width, Height: uint32; format: VkFormat; tiling: VkImageTiling; usage: VkImageUsageFlags; properties: VkMemoryPropertyFlags; out image: VkImage; out imageMemory: VkDeviceMemory);
var
  imageInfo: VkImageCreateInfo;
  memRequirements: VkMemoryRequirements;
  allocInfo: VkMemoryAllocateInfo;
begin
  imageInfo.zero;
  imageInfo.sType := VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
  imageInfo.imageType := VK_IMAGE_TYPE_2D;
  imageInfo.extent.Width := Width;
  imageInfo.extent.Height := Height;
  imageInfo.extent.depth := 1;
  imageInfo.mipLevels := 1;
  imageInfo.arrayLayers := 1;
  imageInfo.format := format;
  imageInfo.tiling := tiling;
  imageInfo.initialLayout := VK_IMAGE_LAYOUT_UNDEFINED;
  imageInfo.usage := usage;
  imageInfo.samples := VK_SAMPLE_COUNT_1_BIT;
  imageInfo.sharingMode := VK_SHARING_MODE_EXCLUSIVE;

  with getVulkan do
    if vkCreateImage(FDevice, @imageInfo, nil, image) <> VK_SUCCESS then
      raise Exception.Create('Impossibile creare immagine');

  getVulkan.vkGetImageMemoryRequirements(FDevice, image, @memRequirements);

  allocInfo.zero;
  allocInfo.sType := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
  allocInfo.allocationSize := memRequirements.size;
  allocInfo.memoryTypeIndex := FindMemoryType(memRequirements.memoryTypeBits, properties);

  if getVulkan.vkAllocateMemory(FDevice, @allocInfo, nil, imageMemory) <> VK_SUCCESS then
    raise Exception.Create('Impossibile allocare memoria immagine');

  getVulkan.vkBindImageMemory(FDevice, image, imageMemory, 0);
end;

procedure TVulkanApplicationTutorial19.TransitionImageLayout(image: VkImage; format: VkFormat; oldLayout, newLayout: VkImageLayout);
var
  allocInfo: VkCommandBufferAllocateInfo;
  commandBuffer: VkCommandBuffer;
  beginInfo: VkCommandBufferBeginInfo;
  barrier: VkImageMemoryBarrier;
  submitInfo: VkSubmitInfo;
begin
  allocInfo.zero;
  allocInfo.sType := VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
  allocInfo.level := VK_COMMAND_BUFFER_LEVEL_PRIMARY;
  allocInfo.commandPool := FCommandPool;
  allocInfo.commandBufferCount := 1;

  with getVulkan do
    vkAllocateCommandBuffers(FDevice, @allocInfo, @commandBuffer);

  beginInfo.zero;
  beginInfo.sType := VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
  beginInfo.flags := VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;

  getVulkan.vkBeginCommandBuffer(commandBuffer, @beginInfo);

  barrier.zero;
  barrier.sType := VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
  barrier.oldLayout := oldLayout;
  barrier.newLayout := newLayout;
  barrier.srcQueueFamilyIndex := VK_QUEUE_FAMILY_IGNORED;
  barrier.dstQueueFamilyIndex := VK_QUEUE_FAMILY_IGNORED;
  barrier.image := image;
  barrier.subresourceRange.aspectMask := VK_IMAGE_ASPECT_COLOR_BIT;
  barrier.subresourceRange.baseMipLevel := 0;
  barrier.subresourceRange.levelCount := 1;
  barrier.subresourceRange.baseArrayLayer := 0;
  barrier.subresourceRange.layerCount := 1;

  // Imposta srcAccessMask e dstAccessMask basati su old/new layout (dal tutorial ufficiale)
  with getVulkan do
  begin
    vkCmdPipelineBarrier(commandBuffer, VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT, VK_PIPELINE_STAGE_TRANSFER_BIT, 0, 0, nil, 0, nil, 1, @barrier);

    vkEndCommandBuffer(commandBuffer);

    submitInfo.zero;
    submitInfo.sType := VK_STRUCTURE_TYPE_SUBMIT_INFO;
    submitInfo.commandBufferCount := 1;
    submitInfo.pCommandBuffers := @commandBuffer;

    vkQueueSubmit(FGraphicsQueue, 1, @submitInfo, VK_NULL_HANDLE);
    vkQueueWaitIdle(FGraphicsQueue);

    vkFreeCommandBuffers(FDevice, FCommandPool, 1, @commandBuffer);

  end;
end;

procedure TVulkanApplicationTutorial19.CopyBufferToImage(buffer: VkBuffer; image: VkImage; Width, Height: uint32);
var
  allocInfo: VkCommandBufferAllocateInfo;
  commandBuffer: VkCommandBuffer;
  beginInfo: VkCommandBufferBeginInfo;
  region: VkBufferImageCopy;
  submitInfo: VkSubmitInfo;
begin
  // Simile a CopyBuffer, ma con vkCmdCopyBufferToImage
  // Implementa come nel tutorial ufficiale
  allocInfo.zero;
  // ... (codice simile a Transition)
  region.bufferOffset := 0;
  region.bufferRowLength := 0;
  region.bufferImageHeight := 0;
  region.imageSubresource.aspectMask := VK_IMAGE_ASPECT_COLOR_BIT;
  region.imageSubresource.mipLevel := 0;
  region.imageSubresource.baseArrayLayer := 0;
  region.imageSubresource.layerCount := 1;
  region.imageOffset.x := 0;
  region.imageOffset.y := 0;
  region.imageOffset.z := 0;
  region.imageExtent.Width := Width;
  region.imageExtent.Height := Height;
  region.imageExtent.depth := 1;

  with getVulkan do
    vkCmdCopyBufferToImage(commandBuffer, buffer, image, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, @region);
  // Submit e wait
end;

procedure TVulkanApplicationTutorial19.CreateTextureImage;
var
  texWidth, texHeight: integer;
  pixels: Pointer;
  imageSize: VkDeviceSize;
  stagingBuffer: VkBuffer;
  stagingBufferMemory: VkDeviceMemory;
begin
  imageSize := LoadImageFromFile('textures/texture.jpg', texWidth, texHeight, pixels);

  CreateBuffer(imageSize, VK_BUFFER_USAGE_TRANSFER_SRC_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT or VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, stagingBuffer, stagingBufferMemory);

  with getVulkan do
  begin
    vkMapMemory(FDevice, stagingBufferMemory, 0, imageSize, 0, @pixels);
    vkUnmapMemory(FDevice, stagingBufferMemory);
  end;

  CreateImage(texWidth, texHeight, VK_FORMAT_R8G8B8A8_SRGB, VK_IMAGE_TILING_OPTIMAL, VK_IMAGE_USAGE_TRANSFER_DST_BIT or VK_IMAGE_USAGE_SAMPLED_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, FTextureImage, FTextureImageMemory);

  TransitionImageLayout(FTextureImage, VK_FORMAT_R8G8B8A8_SRGB, VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL);
  CopyBufferToImage(stagingBuffer, FTextureImage, texWidth, texHeight);
  TransitionImageLayout(FTextureImage, VK_FORMAT_R8G8B8A8_SRGB, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);

  with getVulkan do
  begin
    vkDestroyBuffer(FDevice, stagingBuffer, nil);
    vkFreeMemory(FDevice, stagingBufferMemory, nil);
  end;
  FreeMem(pixels);
end;

procedure TVulkanApplicationTutorial19.CreateTextureImageView;
begin
  FTextureImageView := CreateImageView(FTextureImage, VK_FORMAT_R8G8B8A8_SRGB, VK_IMAGE_ASPECT_COLOR_BIT);
end;

function TVulkanApplicationTutorial19.CreateImageView(image: VkImage; format: VkFormat; aspectFlags: VkImageAspectFlags): VkImageView;
var
  createInfo: VkImageViewCreateInfo;
begin
  createInfo.zero;
  createInfo.sType := VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
  createInfo.image := image;
  createInfo.viewType := VK_IMAGE_VIEW_TYPE_2D;
  createInfo.format := format;
  createInfo.subresourceRange.aspectMask := aspectFlags;
  createInfo.subresourceRange.baseMipLevel := 0;
  createInfo.subresourceRange.levelCount := 1;
  createInfo.subresourceRange.baseArrayLayer := 0;
  createInfo.subresourceRange.layerCount := 1;

  with getVulkan do
    if vkCreateImageView(FDevice, @createInfo, nil, Result) <> VK_SUCCESS then
      raise Exception.Create('Impossibile creare image view');
end;

procedure TVulkanApplicationTutorial19.CreateTextureSampler;
var
  properties: VkPhysicalDeviceProperties;
  samplerInfo: VkSamplerCreateInfo;
begin
  with getVulkan do vkGetPhysicalDeviceProperties(FPhysicalDevice, @properties);

  samplerInfo.zero;
  samplerInfo.sType := VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO;
  samplerInfo.magFilter := VK_FILTER_LINEAR;
  samplerInfo.minFilter := VK_FILTER_LINEAR;
  samplerInfo.addressModeU := VK_SAMPLER_ADDRESS_MODE_REPEAT;
  samplerInfo.addressModeV := VK_SAMPLER_ADDRESS_MODE_REPEAT;
  samplerInfo.addressModeW := VK_SAMPLER_ADDRESS_MODE_REPEAT;
  samplerInfo.anisotropyEnable := VK_TRUE;
  samplerInfo.maxAnisotropy := properties.limits.maxSamplerAnisotropy;
  samplerInfo.borderColor := VK_BORDER_COLOR_INT_OPAQUE_BLACK;
  samplerInfo.unnormalizedCoordinates := VK_FALSE;
  samplerInfo.compareEnable := VK_FALSE;
  samplerInfo.compareOp := VK_COMPARE_OP_ALWAYS;
  samplerInfo.mipmapMode := VK_SAMPLER_MIPMAP_MODE_LINEAR;
  samplerInfo.mipLodBias := 0.0;
  samplerInfo.minLod := 0.0;
  samplerInfo.maxLod := 0.0;

  if getVulkan.vkCreateSampler(FDevice, @samplerInfo, nil, @FTextureSampler) <> VK_SUCCESS then
    raise Exception.Create('Impossibile creare texture sampler');
end;

procedure TVulkanApplicationTutorial19.CreateDescriptorSetLayout;
var
  bindings: array[0..1] of VkDescriptorSetLayoutBinding;
  layoutInfo: VkDescriptorSetLayoutCreateInfo;
begin
  bindings[0].zero;
  bindings[0].binding := 0;
  bindings[0].descriptorType := VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
  bindings[0].descriptorCount := 1;
  bindings[0].stageFlags := VK_SHADER_STAGE_VERTEX_BIT;

  bindings[1].zero;
  bindings[1].binding := 1;
  bindings[1].descriptorType := VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
  bindings[1].descriptorCount := 1;
  bindings[1].stageFlags := VK_SHADER_STAGE_FRAGMENT_BIT;

  layoutInfo.zero;
  layoutInfo.sType := VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
  layoutInfo.bindingCount := 2;
  layoutInfo.pBindings := @bindings[0];

  with getVulkan do
    if vkCreateDescriptorSetLayout(FDevice, @layoutInfo, nil, @FDescriptorSetLayout) <> VK_SUCCESS then
      raise Exception.Create('Impossibile creare descriptor set layout');
end;

procedure TVulkanApplicationTutorial19.CreateDescriptorPool;
var
  poolSizes: array[0..1] of VkDescriptorPoolSize;
  poolInfo: VkDescriptorPoolCreateInfo;
begin
  poolSizes[0].zero;
  poolSizes[0].descriptorType := VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
  poolSizes[0].descriptorCount := MAX_FRAMES_IN_FLIGHT;

  poolSizes[1].zero;
  poolSizes[1].descriptorType := VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
  poolSizes[1].descriptorCount := MAX_FRAMES_IN_FLIGHT;

  poolInfo.zero;
  poolInfo.sType := VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
  poolInfo.poolSizeCount := 2;
  poolInfo.pPoolSizes := @poolSizes[0];
  poolInfo.maxSets := MAX_FRAMES_IN_FLIGHT;

  with getVulkan do
    if vkCreateDescriptorPool(FDevice, @poolInfo, nil, @FDescriptorPool) <> VK_SUCCESS then
      raise Exception.Create('Impossibile creare descriptor pool');
end;

procedure TVulkanApplicationTutorial19.CreateDescriptorSets;
var
  layouts: array of VkDescriptorSetLayout;
  allocInfo: VkDescriptorSetAllocateInfo;
  i: integer;
  descriptorWrites: array[0..1] of VkWriteDescriptorSet;
  bufferInfo: VkDescriptorBufferInfo;
  imageInfo: VkDescriptorImageInfo;
begin
  SetLength(layouts, MAX_FRAMES_IN_FLIGHT);
  for i := 0 to High(layouts) do
    layouts[i] := FDescriptorSetLayout;

  allocInfo.zero;
  allocInfo.sType := VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
  allocInfo.descriptorPool := FDescriptorPool;
  allocInfo.descriptorSetCount := MAX_FRAMES_IN_FLIGHT;
  allocInfo.pSetLayouts := @layouts[0];

  SetLength(FDescriptorSets, MAX_FRAMES_IN_FLIGHT);

  with getVulkan do
    if vkAllocateDescriptorSets(FDevice, @allocInfo, @FDescriptorSets[0]) <> VK_SUCCESS then
      raise Exception.Create('Impossibile allocare descriptor sets');

  for i := 0 to MAX_FRAMES_IN_FLIGHT - 1 do
  begin
    bufferInfo.zero;
    bufferInfo.buffer := FUniformBuffers[i];
    bufferInfo.offset := 0;
    bufferInfo.range := SizeOf(TUniformBufferObject);

    imageInfo.zero;
    imageInfo.imageLayout := VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
    imageInfo.imageView := FTextureImageView;
    imageInfo.sampler := FTextureSampler;

    descriptorWrites[0].zero;
    descriptorWrites[0].sType := VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
    descriptorWrites[0].dstSet := FDescriptorSets[i];
    descriptorWrites[0].dstBinding := 0;
    descriptorWrites[0].dstArrayElement := 0;
    descriptorWrites[0].descriptorType := VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
    descriptorWrites[0].descriptorCount := 1;
    descriptorWrites[0].pBufferInfo := @bufferInfo;

    descriptorWrites[1].zero;
    descriptorWrites[1].sType := VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
    descriptorWrites[1].dstSet := FDescriptorSets[i];
    descriptorWrites[1].dstBinding := 1;
    descriptorWrites[1].dstArrayElement := 0;
    descriptorWrites[1].descriptorType := VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
    descriptorWrites[1].descriptorCount := 1;
    descriptorWrites[1].pImageInfo := @imageInfo;

    getVulkan.vkUpdateDescriptorSets(FDevice, 2, @descriptorWrites[0], 0, nil);
  end;
end;

procedure TVulkanApplicationTutorial19.InitVulkan;
begin
  inherited InitVulkan;
  CreateTextureImage;
  CreateTextureImageView;
  CreateTextureSampler;
end;

procedure TVulkanApplicationTutorial19.Cleanup;
begin
  with getVulkan do
  begin
    vkDestroySampler(FDevice, FTextureSampler, nil);
    vkDestroyImageView(FDevice, FTextureImageView, nil);
    vkDestroyImage(FDevice, FTextureImage, nil);
    vkFreeMemory(FDevice, FTextureImageMemory, nil);
  end;
  inherited Cleanup;
end;

end.
