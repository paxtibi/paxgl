unit vulkan.tutorial.application15;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  pax.vulkan,
  pax.vulkan.helpers,
  vulkan.tutorial,
  vulkan.tutorial.application14;

type
  { TVulkanApplicationTutorial15 }

  TVulkanApplicationTutorial15 = class(TVulkanApplicationTutorial14, IVulkanTutorial)
  protected
    procedure CreateVertexBuffer;
    procedure CreateBuffer(size: VkDeviceSize; usage: VkBufferUsageFlags; properties: VkMemoryPropertyFlags; out buffer: VkBuffer; out bufferMemory: VkDeviceMemory);
    procedure CopyBuffer(srcBuffer: VkBuffer; dstBuffer: VkBuffer; size: VkDeviceSize);
  public
    constructor Create; override;
  end;

implementation

{ TVulkanApplicationTutorial15 }

constructor TVulkanApplicationTutorial15.Create;
begin
  inherited Create;
  Caption := 'Vulkan Tutorial 15 - Staging Buffer';
end;

procedure TVulkanApplicationTutorial15.CreateBuffer(size: VkDeviceSize; usage: VkBufferUsageFlags; properties: VkMemoryPropertyFlags; out buffer: VkBuffer; out bufferMemory: VkDeviceMemory);
var
  bufferInfo: VkBufferCreateInfo;
  memRequirements: VkMemoryRequirements;
  allocInfo: VkMemoryAllocateInfo;
begin
  bufferInfo.zero;
  bufferInfo.sType := VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
  bufferInfo.size := size;
  bufferInfo.usage := usage;
  bufferInfo.sharingMode := VK_SHARING_MODE_EXCLUSIVE;

  with getVulkan do
    if vkCreateBuffer(FDevice, @bufferInfo, nil, buffer) <> VK_SUCCESS then
      raise Exception.Create('Impossibile creare buffer');
  with getVulkan do
  begin
    vkGetBufferMemoryRequirements(FDevice, buffer, @memRequirements);

    allocInfo.zero;
    allocInfo.sType := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
    allocInfo.allocationSize := memRequirements.size;
    allocInfo.memoryTypeIndex := FindMemoryType(memRequirements.memoryTypeBits, properties);

    if vkAllocateMemory(FDevice, @allocInfo, nil, bufferMemory) <> VK_SUCCESS then
      raise Exception.Create('Impossibile allocare memoria buffer');

    vkBindBufferMemory(FDevice, buffer, bufferMemory, 0);
  end;
end;

procedure TVulkanApplicationTutorial15.CopyBuffer(srcBuffer: VkBuffer; dstBuffer: VkBuffer; size: VkDeviceSize);
var
  allocInfo: VkCommandBufferAllocateInfo;
  commandBuffer: VkCommandBuffer;
  beginInfo: VkCommandBufferBeginInfo;
  copyRegion: VkBufferCopy;
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
  with getVulkan do
  begin
    vkBeginCommandBuffer(commandBuffer, @beginInfo);

    copyRegion.size := size;
    vkCmdCopyBuffer(commandBuffer, srcBuffer, dstBuffer, 1, @copyRegion);

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

procedure TVulkanApplicationTutorial15.CreateVertexBuffer;
var
  vertices: array[0..2] of TVertex;
  bufferSize: VkDeviceSize;
  stagingBuffer: VkBuffer;
  stagingBufferMemory: VkDeviceMemory;
  Data: Pointer;
begin
  vertices[0].pos := TVec2.Create(-0.5, -0.5);
  vertices[0].color := TVec3.Create(1.0, 0.0, 0.0);
  vertices[1].pos := TVec2.Create(0.5, -0.5);
  vertices[1].color := TVec3.Create(0.0, 1.0, 0.0);
  vertices[2].pos := TVec2.Create(0.0, 0.5);
  vertices[2].color := TVec3.Create(0.0, 0.0, 1.0);

  bufferSize := SizeOf(vertices);

  CreateBuffer(bufferSize, VK_BUFFER_USAGE_TRANSFER_SRC_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT or VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, stagingBuffer, stagingBufferMemory);

  with getVulkan do
  begin
    vkMapMemory(FDevice, stagingBufferMemory, 0, bufferSize, 0, @Data);
    Move(vertices[0], Data^, bufferSize);
    vkUnmapMemory(FDevice, stagingBufferMemory);
  end;

  CreateBuffer(bufferSize, VK_BUFFER_USAGE_TRANSFER_DST_BIT or VK_BUFFER_USAGE_VERTEX_BUFFER_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, FVertexBuffer, FVertexBufferMemory);

  CopyBuffer(stagingBuffer, FVertexBuffer, bufferSize);

  with getVulkan do
  begin
    vkDestroyBuffer(FDevice, stagingBuffer, nil);
    vkFreeMemory(FDevice, stagingBufferMemory, nil);
  end;
end;

end.
