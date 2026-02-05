unit pax.vulkan.helpers;

{$mode objfpc}{$H+}
{$ModeSwitch typehelpers}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, pax.vulkan;

type
  TVkApplicationInfoHelper = type helper for VkApplicationInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkInstanceCreateInfoHelper = type helper for VkInstanceCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkDebugUtilsMessengerCreateInfoEXTHelper = type helper for VkDebugUtilsMessengerCreateInfoEXT
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkPhysicalDeviceFeaturesHelper = type helper for VkPhysicalDeviceFeatures
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkDeviceQueueCreateInfoHelper = type helper for VkDeviceQueueCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkDeviceCreateInfoHelper = type helper for VkDeviceCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkSurfaceCapabilitiesKHRHelper = type helper for VkSurfaceCapabilitiesKHR
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkSurfaceFormatKHRHelper = type helper for VkSurfaceFormatKHR
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkPresentModeKHRHelper = type helper for VkPresentModeKHR
  public
    function ToString: string;
  end;

  TVkSwapchainCreateInfoKHRHelper = type helper for VkSwapchainCreateInfoKHR
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkImageViewCreateInfoHelper = type helper for VkImageViewCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkAttachmentDescriptionHelper = type helper for VkAttachmentDescription
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkAttachmentReferenceHelper = type helper for VkAttachmentReference
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkSubpassDescriptionHelper = type helper for VkSubpassDescription
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkRenderPassCreateInfoHelper = type helper for VkRenderPassCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkFramebufferCreateInfoHelper = type helper for VkFramebufferCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkShaderModuleCreateInfoHelper = type helper for VkShaderModuleCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkPipelineShaderStageCreateInfoHelper = type helper for VkPipelineShaderStageCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkPipelineVertexInputStateCreateInfoHelper = type helper for VkPipelineVertexInputStateCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkPipelineInputAssemblyStateCreateInfoHelper = type helper for VkPipelineInputAssemblyStateCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkPipelineViewportStateCreateInfoHelper = type helper for VkPipelineViewportStateCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkPipelineRasterizationStateCreateInfoHelper = type helper for VkPipelineRasterizationStateCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkPipelineMultisampleStateCreateInfoHelper = type helper for VkPipelineMultisampleStateCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkPipelineColorBlendAttachmentStateHelper = type helper for VkPipelineColorBlendAttachmentState
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkPipelineColorBlendStateCreateInfoHelper = type helper for VkPipelineColorBlendStateCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkPipelineDynamicStateCreateInfoHelper = type helper for VkPipelineDynamicStateCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkPipelineLayoutCreateInfoHelper = type helper for VkPipelineLayoutCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkGraphicsPipelineCreateInfoHelper = type helper for VkGraphicsPipelineCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkCommandPoolCreateInfoHelper = type helper for VkCommandPoolCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkCommandBufferAllocateInfoHelper = type helper for VkCommandBufferAllocateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkCommandBufferBeginInfoHelper = type helper for VkCommandBufferBeginInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkRenderPassBeginInfoHelper = type helper for VkRenderPassBeginInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkSemaphoreCreateInfoHelper = type helper for VkSemaphoreCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkFenceCreateInfoHelper = type helper for VkFenceCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkSubmitInfoHelper = type helper for VkSubmitInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkPresentInfoKHRHelper = type helper for VkPresentInfoKHR
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkVertexInputBindingDescriptionHelper = type helper for VkVertexInputBindingDescription
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkVertexInputAttributeDescriptionHelper = type helper for VkVertexInputAttributeDescription
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkBufferCreateInfoHelper = type helper for VkBufferCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkMemoryAllocateInfoHelper = type helper for VkMemoryAllocateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkDescriptorSetLayoutBindingHelper = type helper for VkDescriptorSetLayoutBinding
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkDescriptorSetLayoutCreateInfoHelper = type helper for VkDescriptorSetLayoutCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkDescriptorPoolSizeHelper = type helper for VkDescriptorPoolSize
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkDescriptorPoolCreateInfoHelper = type helper for VkDescriptorPoolCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkDescriptorSetAllocateInfoHelper = type helper for VkDescriptorSetAllocateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkWriteDescriptorSetHelper = type helper for VkWriteDescriptorSet
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkDescriptorBufferInfoHelper = type helper for VkDescriptorBufferInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkImageCreateInfoHelper = type helper for VkImageCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkImageMemoryBarrierHelper = type helper for VkImageMemoryBarrier
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkBufferCopyHelper = type helper for VkBufferCopy
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkBufferImageCopyHelper = type helper for VkBufferImageCopy
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkSamplerCreateInfoHelper = type helper for VkSamplerCreateInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  TVkDescriptorImageInfoHelper = type helper for VkDescriptorImageInfo
  public
    procedure zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;

  { TVkSubpassDependencyHelper }

  TVkSubpassDependencyHelper = type helper for VkSubpassDependency
  public
    procedure Zero;
    function ToString: string;
    function ToFormattedString(const Indent: string = '  '): string;
  end;


operator := (i: longword): boolean; inline;
operator := (ai: VkApplicationInfo): string;
operator := (ici: VkInstanceCreateInfo): string;
operator := (aResult: VkResult): string;
operator := (aFormat: VkFormat): string;
operator := (sType: VkStructureType): string; inline;

procedure CheckVulkanResult(Result: VkResult; const operation: string);

type
  EVulkanError = class(Exception)

  end;

implementation

uses
  typinfo;

procedure CheckVulkanResult(Result: VkResult; const operation: string);
begin
  if Result <> VK_SUCCESS then
    raise EVulkanError.CreateFmt('%s fallito: %s', [operation, Result]);
end;

function IfThen(condition: boolean; ifTrue, ifFalse: string): string;
begin
  if condition then
    Result := ifTrue
  else
    Result := ifFalse;
end;

operator := (aResult: VkResult): string;
begin
  Result := GetEnumName(TypeInfo(VkResult), Ord(aResult));
end;

operator := (aFormat: VkFormat): string;
begin
  Result := GetEnumName(TypeInfo(VkFormat), Ord(aFormat));
end;

operator := (Value: VkDynamicState): string; inline;
begin
  case Value of
    VK_DYNAMIC_STATE_VIEWPORT: Result := 'VIEWPORT';
    VK_DYNAMIC_STATE_SCISSOR: Result := 'SCISSOR';
    VK_DYNAMIC_STATE_LINE_WIDTH: Result := 'LINE_WIDTH';
    VK_DYNAMIC_STATE_DEPTH_BIAS: Result := 'DEPTH_BIAS';
    VK_DYNAMIC_STATE_BLEND_CONSTANTS: Result := 'BLEND_CONSTANTS';
    VK_DYNAMIC_STATE_DEPTH_BOUNDS: Result := 'DEPTH_BOUNDS';
    VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK: Result := 'STENCIL_COMPARE_MASK';
    VK_DYNAMIC_STATE_STENCIL_WRITE_MASK: Result := 'STENCIL_WRITE_MASK';
    VK_DYNAMIC_STATE_STENCIL_REFERENCE: Result := 'STENCIL_REFERENCE';

    // Extended dynamic state (Vulkan 1.2 / EXT_extended_dynamic_state)
    VK_DYNAMIC_STATE_CULL_MODE: Result := 'CULL_MODE';
    VK_DYNAMIC_STATE_FRONT_FACE: Result := 'FRONT_FACE';
    VK_DYNAMIC_STATE_PRIMITIVE_TOPOLOGY: Result := 'PRIMITIVE_TOPOLOGY';
    VK_DYNAMIC_STATE_VIEWPORT_WITH_COUNT: Result := 'VIEWPORT_WITH_COUNT';
    VK_DYNAMIC_STATE_SCISSOR_WITH_COUNT: Result := 'SCISSOR_WITH_COUNT';
    VK_DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE: Result := 'VERTEX_INPUT_BINDING_STRIDE';
    VK_DYNAMIC_STATE_DEPTH_TEST_ENABLE: Result := 'DEPTH_TEST_ENABLE';
    VK_DYNAMIC_STATE_DEPTH_WRITE_ENABLE: Result := 'DEPTH_WRITE_ENABLE';
    VK_DYNAMIC_STATE_DEPTH_COMPARE_OP: Result := 'DEPTH_COMPARE_OP';
    VK_DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE: Result := 'DEPTH_BOUNDS_TEST_ENABLE';
    VK_DYNAMIC_STATE_STENCIL_TEST_ENABLE: Result := 'STENCIL_TEST_ENABLE';
    VK_DYNAMIC_STATE_STENCIL_OP: Result := 'STENCIL_OP';

    // Vulkan 1.3 / EXT_extended_dynamic_state3
    VK_DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE: Result := 'RASTERIZER_DISCARD_ENABLE';
    VK_DYNAMIC_STATE_DEPTH_BIAS_ENABLE: Result := 'DEPTH_BIAS_ENABLE';
    VK_DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE: Result := 'PRIMITIVE_RESTART_ENABLE';

    // Altri meno comuni
    VK_DYNAMIC_STATE_DISCARD_RECTANGLE_ENABLE_EXT: Result := 'DISCARD_RECTANGLE_ENABLE_EXT';
    VK_DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR: Result := 'FRAGMENT_SHADING_RATE_KHR';
    VK_DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT: Result := 'COLOR_WRITE_ENABLE_EXT';

    else
      Result := Format('VkDynamicState(%d)', [Ord(Value)]);
  end;
end;

operator := (sType: VkStructureType): string;
begin
  case sType of
    VK_STRUCTURE_TYPE_APPLICATION_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_APPLICATION_INFO';
    end;
    VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_SUBMIT_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_SUBMIT_INFO';
    end;
    VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO';
    end;
    VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE: begin
      Result := 'VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE';
    end;
    VK_STRUCTURE_TYPE_BIND_SPARSE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_BIND_SPARSE_INFO';
    end;
    VK_STRUCTURE_TYPE_FENCE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_FENCE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_EVENT_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_EVENT_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO';
    end;
    VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET: begin
      Result := 'VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET';
    end;
    VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET: begin
      Result := 'VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET';
    end;
    VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO';
    end;
    VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO';
    end;
    VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO';
    end;
    VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO';
    end;
    VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER: begin
      Result := 'VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER';
    end;
    VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER';
    end;
    VK_STRUCTURE_TYPE_MEMORY_BARRIER: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_BARRIER';
    end;
    VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO';
    end;
    VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO';
    end;
    VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS';
    end;
    VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO';
    end;
    VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO';
    end;
    VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO';
    end;
    VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO';
    end;
    VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO';
    end;
    VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO';
    end;
    VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2: begin
      Result := 'VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2';
    end;
    VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2';
    end;
    VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2';
    end;
    VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2';
    end;
    VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2: begin
      Result := 'VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2';
    end;
    VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2: begin
      Result := 'VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2';
    end;
    VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2';
    end;
    VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2: begin
      Result := 'VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2';
    end;
    VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2: begin
      Result := 'VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2';
    end;
    VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO';
    end;
    VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO';
    end;
    VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO';
    end;
    VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO';
    end;
    VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES';
    end;
    VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT: begin
      Result := 'VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT';
    end;
    VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO';
    end;
    VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO';
    end;
    VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES';
    end;
    VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO';
    end;
    VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO';
    end;
    VK_STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES';
    end;
    VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO';
    end;
    VK_STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO';
    end;
    VK_STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO';
    end;
    VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT: begin
      Result := 'VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES';
    end;
    VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2: begin
      Result := 'VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2';
    end;
    VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2: begin
      Result := 'VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2';
    end;
    VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2: begin
      Result := 'VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2';
    end;
    VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2: begin
      Result := 'VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2';
    end;
    VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2: begin
      Result := 'VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2';
    end;
    VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO';
    end;
    VK_STRUCTURE_TYPE_SUBPASS_END_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_SUBPASS_END_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE: begin
      Result := 'VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE';
    end;
    VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES';
    end;
    VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO';
    end;
    VK_STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES';
    end;
    VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT: begin
      Result := 'VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT';
    end;
    VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT: begin
      Result := 'VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES';
    end;
    VK_STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_MEMORY_BARRIER_2: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_BARRIER_2';
    end;
    VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER_2: begin
      Result := 'VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER_2';
    end;
    VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2';
    end;
    VK_STRUCTURE_TYPE_DEPENDENCY_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_DEPENDENCY_INFO';
    end;
    VK_STRUCTURE_TYPE_SUBMIT_INFO_2: begin
      Result := 'VK_STRUCTURE_TYPE_SUBMIT_INFO_2';
    end;
    VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO';
    end;
    VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SYNCHRONIZATION_2_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SYNCHRONIZATION_2_FEATURES';
    end;
    VK_STRUCTURE_TYPE_COPY_BUFFER_INFO_2: begin
      Result := 'VK_STRUCTURE_TYPE_COPY_BUFFER_INFO_2';
    end;
    VK_STRUCTURE_TYPE_COPY_IMAGE_INFO_2: begin
      Result := 'VK_STRUCTURE_TYPE_COPY_IMAGE_INFO_2';
    end;
    VK_STRUCTURE_TYPE_COPY_BUFFER_TO_IMAGE_INFO_2: begin
      Result := 'VK_STRUCTURE_TYPE_COPY_BUFFER_TO_IMAGE_INFO_2';
    end;
    VK_STRUCTURE_TYPE_COPY_IMAGE_TO_BUFFER_INFO_2: begin
      Result := 'VK_STRUCTURE_TYPE_COPY_IMAGE_TO_BUFFER_INFO_2';
    end;
    VK_STRUCTURE_TYPE_BUFFER_COPY_2: begin
      Result := 'VK_STRUCTURE_TYPE_BUFFER_COPY_2';
    end;
    VK_STRUCTURE_TYPE_IMAGE_COPY_2: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_COPY_2';
    end;
    VK_STRUCTURE_TYPE_BUFFER_IMAGE_COPY_2: begin
      Result := 'VK_STRUCTURE_TYPE_BUFFER_IMAGE_COPY_2';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES';
    end;
    VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_3: begin
      Result := 'VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_3';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_DEVICE_BUFFER_MEMORY_REQUIREMENTS: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_BUFFER_MEMORY_REQUIREMENTS';
    end;
    VK_STRUCTURE_TYPE_DEVICE_IMAGE_MEMORY_REQUIREMENTS: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_IMAGE_MEMORY_REQUIREMENTS';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TERMINATE_INVOCATION_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TERMINATE_INVOCATION_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_WORKGROUP_MEMORY_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_WORKGROUP_MEMORY_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ROBUSTNESS_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ROBUSTNESS_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK: begin
      Result := 'VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK';
    end;
    VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_BLIT_IMAGE_INFO_2: begin
      Result := 'VK_STRUCTURE_TYPE_BLIT_IMAGE_INFO_2';
    end;
    VK_STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2: begin
      Result := 'VK_STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2';
    end;
    VK_STRUCTURE_TYPE_IMAGE_BLIT_2: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_BLIT_2';
    end;
    VK_STRUCTURE_TYPE_IMAGE_RESOLVE_2: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_RESOLVE_2';
    end;
    VK_STRUCTURE_TYPE_RENDERING_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_RENDERING_INFO';
    end;
    VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES';
    end;
    VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDERING_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDERING_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_4_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_4_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_4_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_4_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GLOBAL_PRIORITY_QUERY_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GLOBAL_PRIORITY_QUERY_FEATURES';
    end;
    VK_STRUCTURE_TYPE_QUEUE_FAMILY_GLOBAL_PRIORITY_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_QUEUE_FAMILY_GLOBAL_PRIORITY_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES';
    end;
    VK_STRUCTURE_TYPE_MEMORY_MAP_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_MAP_INFO';
    end;
    VK_STRUCTURE_TYPE_MEMORY_UNMAP_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_UNMAP_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_5_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_5_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_5_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_5_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_DEVICE_IMAGE_SUBRESOURCE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_IMAGE_SUBRESOURCE_INFO';
    end;
    VK_STRUCTURE_TYPE_SUBRESOURCE_LAYOUT_2: begin
      Result := 'VK_STRUCTURE_TYPE_SUBRESOURCE_LAYOUT_2';
    end;
    VK_STRUCTURE_TYPE_IMAGE_SUBRESOURCE_2: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_SUBRESOURCE_2';
    end;
    VK_STRUCTURE_TYPE_BUFFER_USAGE_FLAGS_2_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_BUFFER_USAGE_FLAGS_2_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_BIND_MEMORY_STATUS: begin
      Result := 'VK_STRUCTURE_TYPE_BIND_MEMORY_STATUS';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_MEMORY_TO_IMAGE_COPY: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_TO_IMAGE_COPY';
    end;
    VK_STRUCTURE_TYPE_IMAGE_TO_MEMORY_COPY: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_TO_MEMORY_COPY';
    end;
    VK_STRUCTURE_TYPE_COPY_IMAGE_TO_MEMORY_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_COPY_IMAGE_TO_MEMORY_INFO';
    end;
    VK_STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INFO';
    end;
    VK_STRUCTURE_TYPE_HOST_IMAGE_LAYOUT_TRANSITION_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_HOST_IMAGE_LAYOUT_TRANSITION_INFO';
    end;
    VK_STRUCTURE_TYPE_COPY_IMAGE_TO_IMAGE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_COPY_IMAGE_TO_IMAGE_INFO';
    end;
    VK_STRUCTURE_TYPE_SUBRESOURCE_HOST_MEMCPY_SIZE: begin
      Result := 'VK_STRUCTURE_TYPE_SUBRESOURCE_HOST_MEMCPY_SIZE';
    end;
    VK_STRUCTURE_TYPE_HOST_IMAGE_COPY_DEVICE_PERFORMANCE_QUERY: begin
      Result := 'VK_STRUCTURE_TYPE_HOST_IMAGE_COPY_DEVICE_PERFORMANCE_QUERY';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_ROTATE_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_ROTATE_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT_CONTROLS_2_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT_CONTROLS_2_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_EXPECT_ASSUME_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_EXPECT_ASSUME_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_CREATE_FLAGS_2_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_CREATE_FLAGS_2_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_BIND_DESCRIPTOR_SETS_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_BIND_DESCRIPTOR_SETS_INFO';
    end;
    VK_STRUCTURE_TYPE_PUSH_CONSTANTS_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PUSH_CONSTANTS_INFO';
    end;
    VK_STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_INFO';
    end;
    VK_STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_WITH_TEMPLATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_WITH_TEMPLATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_PROTECTED_ACCESS_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_PROTECTED_ACCESS_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_ROBUSTNESS_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_ROBUSTNESS_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES';
    end;
    VK_STRUCTURE_TYPE_RENDERING_AREA_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_RENDERING_AREA_INFO';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_LOCAL_READ_FEATURES: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_LOCAL_READ_FEATURES';
    end;
    VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_LOCATION_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_LOCATION_INFO';
    end;
    VK_STRUCTURE_TYPE_RENDERING_INPUT_ATTACHMENT_INDEX_INFO: begin
      Result := 'VK_STRUCTURE_TYPE_RENDERING_INPUT_ATTACHMENT_INDEX_INFO';
    end;
    VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PRESENT_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PRESENT_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR';
    end;
    VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD';
    end;
    VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_VIDEO_PROFILE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_PROFILE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_CAPABILITIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_CAPABILITIES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_PICTURE_RESOURCE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_PICTURE_RESOURCE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_SESSION_MEMORY_REQUIREMENTS_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_SESSION_MEMORY_REQUIREMENTS_KHR';
    end;
    VK_STRUCTURE_TYPE_BIND_VIDEO_SESSION_MEMORY_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_BIND_VIDEO_SESSION_MEMORY_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_SESSION_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_SESSION_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_SESSION_PARAMETERS_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_SESSION_PARAMETERS_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_SESSION_PARAMETERS_UPDATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_SESSION_PARAMETERS_UPDATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_BEGIN_CODING_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_BEGIN_CODING_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_END_CODING_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_END_CODING_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_CODING_CONTROL_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_CODING_CONTROL_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_REFERENCE_SLOT_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_REFERENCE_SLOT_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_QUEUE_FAMILY_VIDEO_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_QUEUE_FAMILY_VIDEO_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_PROFILE_LIST_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_PROFILE_LIST_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VIDEO_FORMAT_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VIDEO_FORMAT_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_FORMAT_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_FORMAT_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_QUEUE_FAMILY_QUERY_RESULT_STATUS_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_QUEUE_FAMILY_QUERY_RESULT_STATUS_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_CAPABILITIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_CAPABILITIES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_USAGE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_USAGE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_CU_MODULE_CREATE_INFO_NVX: begin
      Result := 'VK_STRUCTURE_TYPE_CU_MODULE_CREATE_INFO_NVX';
    end;
    VK_STRUCTURE_TYPE_CU_FUNCTION_CREATE_INFO_NVX: begin
      Result := 'VK_STRUCTURE_TYPE_CU_FUNCTION_CREATE_INFO_NVX';
    end;
    VK_STRUCTURE_TYPE_CU_LAUNCH_INFO_NVX: begin
      Result := 'VK_STRUCTURE_TYPE_CU_LAUNCH_INFO_NVX';
    end;
    VK_STRUCTURE_TYPE_CU_MODULE_TEXTURING_MODE_CREATE_INFO_NVX: begin
      Result := 'VK_STRUCTURE_TYPE_CU_MODULE_TEXTURING_MODE_CREATE_INFO_NVX';
    end;
    VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX';
    end;
    VK_STRUCTURE_TYPE_IMAGE_VIEW_ADDRESS_PROPERTIES_NVX: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_VIEW_ADDRESS_PROPERTIES_NVX';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_CAPABILITIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_CAPABILITIES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_SESSION_PARAMETERS_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_SESSION_PARAMETERS_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_SESSION_PARAMETERS_ADD_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_SESSION_PARAMETERS_ADD_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_PICTURE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_PICTURE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_DPB_SLOT_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_DPB_SLOT_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_NALU_SLICE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_NALU_SLICE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_GOP_REMAINING_FRAME_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_GOP_REMAINING_FRAME_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_PROFILE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_PROFILE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_RATE_CONTROL_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_RATE_CONTROL_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_RATE_CONTROL_LAYER_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_RATE_CONTROL_LAYER_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_SESSION_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_SESSION_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_QUALITY_LEVEL_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_QUALITY_LEVEL_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_SESSION_PARAMETERS_GET_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_SESSION_PARAMETERS_GET_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_SESSION_PARAMETERS_FEEDBACK_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_SESSION_PARAMETERS_FEEDBACK_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_CAPABILITIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_CAPABILITIES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_SESSION_PARAMETERS_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_SESSION_PARAMETERS_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_SESSION_PARAMETERS_ADD_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_SESSION_PARAMETERS_ADD_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_PICTURE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_PICTURE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_DPB_SLOT_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_DPB_SLOT_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_NALU_SLICE_SEGMENT_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_NALU_SLICE_SEGMENT_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_GOP_REMAINING_FRAME_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_GOP_REMAINING_FRAME_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_PROFILE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_PROFILE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_RATE_CONTROL_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_RATE_CONTROL_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_RATE_CONTROL_LAYER_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_RATE_CONTROL_LAYER_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_SESSION_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_SESSION_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_QUALITY_LEVEL_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_QUALITY_LEVEL_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_SESSION_PARAMETERS_GET_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_SESSION_PARAMETERS_GET_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_SESSION_PARAMETERS_FEEDBACK_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_SESSION_PARAMETERS_FEEDBACK_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_H264_CAPABILITIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_H264_CAPABILITIES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_H264_PICTURE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_H264_PICTURE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_H264_PROFILE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_H264_PROFILE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_H264_SESSION_PARAMETERS_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_H264_SESSION_PARAMETERS_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_H264_SESSION_PARAMETERS_ADD_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_H264_SESSION_PARAMETERS_ADD_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_H264_DPB_SLOT_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_H264_DPB_SLOT_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD: begin
      Result := 'VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD';
    end;
    VK_STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP: begin
      Result := 'VK_STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT';
    end;
    VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN: begin
      Result := 'VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN';
    end;
    VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT';
    end;
    VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE: begin
      Result := 'VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX';
    end;
    VK_STRUCTURE_TYPE_MULTIVIEW_PER_VIEW_ATTRIBUTES_INFO_NVX: begin
      Result := 'VK_STRUCTURE_TYPE_MULTIVIEW_PER_VIEW_ATTRIBUTES_INFO_NVX';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_HDR_METADATA_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_HDR_METADATA_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RELAXED_LINE_RASTERIZATION_FEATURES_IMG: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RELAXED_LINE_RASTERIZATION_FEATURES_IMG';
    end;
    VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR';
    end;
    VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PERFORMANCE_QUERY_SUBMIT_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PERFORMANCE_QUERY_SUBMIT_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_ACQUIRE_PROFILING_LOCK_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_ACQUIRE_PROFILING_LOCK_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PERFORMANCE_COUNTER_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PERFORMANCE_COUNTER_KHR';
    end;
    VK_STRUCTURE_TYPE_PERFORMANCE_COUNTER_DESCRIPTION_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PERFORMANCE_COUNTER_DESCRIPTION_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR';
    end;
    VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR';
    end;
    VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR';
    end;
    VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR';
    end;
    VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR';
    end;
    VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR';
    end;
    VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR';
    end;
    VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR';
    end;
    VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK: begin
      Result := 'VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK';
    end;
    VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK: begin
      Result := 'VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK';
    end;
    VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT';
    end;
    VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT';
    end;
    VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID: begin
      Result := 'VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID';
    end;
    VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID: begin
      Result := 'VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID';
    end;
    VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID: begin
      Result := 'VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID';
    end;
    VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID: begin
      Result := 'VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID';
    end;
    VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID';
    end;
    VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID: begin
      Result := 'VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID';
    end;
    VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_2_ANDROID: begin
      Result := 'VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_2_ANDROID';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ENQUEUE_FEATURES_AMDX: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ENQUEUE_FEATURES_AMDX';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ENQUEUE_PROPERTIES_AMDX: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ENQUEUE_PROPERTIES_AMDX';
    end;
    VK_STRUCTURE_TYPE_EXECUTION_GRAPH_PIPELINE_SCRATCH_SIZE_AMDX: begin
      Result := 'VK_STRUCTURE_TYPE_EXECUTION_GRAPH_PIPELINE_SCRATCH_SIZE_AMDX';
    end;
    VK_STRUCTURE_TYPE_EXECUTION_GRAPH_PIPELINE_CREATE_INFO_AMDX: begin
      Result := 'VK_STRUCTURE_TYPE_EXECUTION_GRAPH_PIPELINE_CREATE_INFO_AMDX';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_NODE_CREATE_INFO_AMDX: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_NODE_CREATE_INFO_AMDX';
    end;
    VK_STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_AMD: begin
      Result := 'VK_STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_AMD';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_BFLOAT16_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_BFLOAT16_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR';
    end;
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR';
    end;
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR';
    end;
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR';
    end;
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR';
    end;
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_QUERY_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_QUERY_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_PROPERTIES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_PROPERTIES_NV';
    end;
    VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_2_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_2_EXT';
    end;
    VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PORTABILITY_SUBSET_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PORTABILITY_SUBSET_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PORTABILITY_SUBSET_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PORTABILITY_SUBSET_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_GEOMETRY_NV: begin
      Result := 'VK_STRUCTURE_TYPE_GEOMETRY_NV';
    end;
    VK_STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV';
    end;
    VK_STRUCTURE_TYPE_GEOMETRY_AABB_NV: begin
      Result := 'VK_STRUCTURE_TYPE_GEOMETRY_AABB_NV';
    end;
    VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV: begin
      Result := 'VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV';
    end;
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV';
    end;
    VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CLOCK_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CLOCK_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_COMPILER_CONTROL_CREATE_INFO_AMD: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_COMPILER_CONTROL_CREATE_INFO_AMD';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_H265_CAPABILITIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_H265_CAPABILITIES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_H265_SESSION_PARAMETERS_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_H265_SESSION_PARAMETERS_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_H265_SESSION_PARAMETERS_ADD_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_H265_SESSION_PARAMETERS_ADD_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_H265_PROFILE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_H265_PROFILE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_H265_PICTURE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_H265_PICTURE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_H265_DPB_SLOT_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_H265_DPB_SLOT_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP: begin
      Result := 'VK_STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_CHECKPOINT_DATA_NV: begin
      Result := 'VK_STRUCTURE_TYPE_CHECKPOINT_DATA_NV';
    end;
    VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV';
    end;
    VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_2_NV: begin
      Result := 'VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_2_NV';
    end;
    VK_STRUCTURE_TYPE_CHECKPOINT_DATA_2_NV: begin
      Result := 'VK_STRUCTURE_TYPE_CHECKPOINT_DATA_2_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_TIMING_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_TIMING_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_SWAPCHAIN_TIMING_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_SWAPCHAIN_TIMING_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_SWAPCHAIN_TIME_DOMAIN_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_SWAPCHAIN_TIME_DOMAIN_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_PRESENT_TIMINGS_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PRESENT_TIMINGS_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PRESENT_TIMING_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PRESENT_TIMING_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PAST_PRESENTATION_TIMING_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PAST_PRESENTATION_TIMING_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PAST_PRESENTATION_TIMING_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PAST_PRESENTATION_TIMING_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_PAST_PRESENTATION_TIMING_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PAST_PRESENTATION_TIMING_EXT';
    end;
    VK_STRUCTURE_TYPE_PRESENT_TIMING_SURFACE_CAPABILITIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PRESENT_TIMING_SURFACE_CAPABILITIES_EXT';
    end;
    VK_STRUCTURE_TYPE_SWAPCHAIN_CALIBRATED_TIMESTAMP_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_SWAPCHAIN_CALIBRATED_TIMESTAMP_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_FUNCTIONS_2_FEATURES_INTEL: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_FUNCTIONS_2_FEATURES_INTEL';
    end;
    VK_STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_QUERY_CREATE_INFO_INTEL: begin
      Result := 'VK_STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_QUERY_CREATE_INFO_INTEL';
    end;
    VK_STRUCTURE_TYPE_INITIALIZE_PERFORMANCE_API_INFO_INTEL: begin
      Result := 'VK_STRUCTURE_TYPE_INITIALIZE_PERFORMANCE_API_INFO_INTEL';
    end;
    VK_STRUCTURE_TYPE_PERFORMANCE_MARKER_INFO_INTEL: begin
      Result := 'VK_STRUCTURE_TYPE_PERFORMANCE_MARKER_INFO_INTEL';
    end;
    VK_STRUCTURE_TYPE_PERFORMANCE_STREAM_MARKER_INFO_INTEL: begin
      Result := 'VK_STRUCTURE_TYPE_PERFORMANCE_STREAM_MARKER_INFO_INTEL';
    end;
    VK_STRUCTURE_TYPE_PERFORMANCE_OVERRIDE_INFO_INTEL: begin
      Result := 'VK_STRUCTURE_TYPE_PERFORMANCE_OVERRIDE_INFO_INTEL';
    end;
    VK_STRUCTURE_TYPE_PERFORMANCE_CONFIGURATION_ACQUIRE_INFO_INTEL: begin
      Result := 'VK_STRUCTURE_TYPE_PERFORMANCE_CONFIGURATION_ACQUIRE_INFO_INTEL';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD: begin
      Result := 'VK_STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD';
    end;
    VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD: begin
      Result := 'VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD';
    end;
    VK_STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA';
    end;
    VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_STATE_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_STATE_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_KHR';
    end;
    VK_STRUCTURE_TYPE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_2_AMD: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_2_AMD';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_ATOMIC_INT64_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_ATOMIC_INT64_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_QUAD_CONTROL_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_QUAD_CONTROL_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_WAIT_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_WAIT_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COVERAGE_REDUCTION_MODE_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COVERAGE_REDUCTION_MODE_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_REDUCTION_STATE_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_REDUCTION_STATE_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_FRAMEBUFFER_MIXED_SAMPLES_COMBINATION_NV: begin
      Result := 'VK_STRUCTURE_TYPE_FRAMEBUFFER_MIXED_SAMPLES_COMBINATION_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_INTERLOCK_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_INTERLOCK_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROVOKING_VERTEX_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROVOKING_VERTEX_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_PROVOKING_VERTEX_STATE_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_PROVOKING_VERTEX_STATE_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROVOKING_VERTEX_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROVOKING_VERTEX_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT';
    end;
    VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAP_MEMORY_PLACED_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAP_MEMORY_PLACED_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAP_MEMORY_PLACED_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAP_MEMORY_PLACED_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_MEMORY_MAP_PLACED_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_MAP_PLACED_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT_2_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT_2_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV';
    end;
    VK_STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV: begin
      Result := 'VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV';
    end;
    VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INHERITED_VIEWPORT_SCISSOR_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INHERITED_VIEWPORT_SCISSOR_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_VIEWPORT_SCISSOR_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_VIEWPORT_SCISSOR_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM';
    end;
    VK_STRUCTURE_TYPE_RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_BIAS_CONTROL_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_BIAS_CONTROL_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_DEPTH_BIAS_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DEPTH_BIAS_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_DEPTH_BIAS_REPRESENTATION_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DEPTH_BIAS_REPRESENTATION_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_MEMORY_REPORT_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_MEMORY_REPORT_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_DEVICE_DEVICE_MEMORY_REPORT_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_DEVICE_MEMORY_REPORT_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_DEVICE_MEMORY_REPORT_CALLBACK_DATA_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_MEMORY_REPORT_CALLBACK_DATA_EXT';
    end;
    VK_STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_3D_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_3D_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_BARRIER_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_BARRIER_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_PRESENT_BARRIER_NV: begin
      Result := 'VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_PRESENT_BARRIER_NV';
    end;
    VK_STRUCTURE_TYPE_SWAPCHAIN_PRESENT_BARRIER_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_SWAPCHAIN_PRESENT_BARRIER_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_PRESENT_ID_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PRESENT_ID_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_ID_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_ID_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_RATE_CONTROL_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_RATE_CONTROL_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_RATE_CONTROL_LAYER_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_RATE_CONTROL_LAYER_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_CAPABILITIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_CAPABILITIES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_USAGE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_USAGE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_QUERY_POOL_VIDEO_ENCODE_FEEDBACK_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_QUERY_POOL_VIDEO_ENCODE_FEEDBACK_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VIDEO_ENCODE_QUALITY_LEVEL_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VIDEO_ENCODE_QUALITY_LEVEL_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_QUALITY_LEVEL_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_QUALITY_LEVEL_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_QUALITY_LEVEL_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_QUALITY_LEVEL_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_SESSION_PARAMETERS_GET_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_SESSION_PARAMETERS_GET_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_SESSION_PARAMETERS_FEEDBACK_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_SESSION_PARAMETERS_FEEDBACK_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DIAGNOSTICS_CONFIG_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DIAGNOSTICS_CONFIG_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_DEVICE_DIAGNOSTICS_CONFIG_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_DIAGNOSTICS_CONFIG_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_CUDA_MODULE_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_CUDA_MODULE_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_CUDA_FUNCTION_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_CUDA_FUNCTION_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_CUDA_LAUNCH_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_CUDA_LAUNCH_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUDA_KERNEL_LAUNCH_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUDA_KERNEL_LAUNCH_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUDA_KERNEL_LAUNCH_PROPERTIES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUDA_KERNEL_LAUNCH_PROPERTIES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_SHADING_FEATURES_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_SHADING_FEATURES_QCOM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_SHADING_PROPERTIES_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_SHADING_PROPERTIES_QCOM';
    end;
    VK_STRUCTURE_TYPE_RENDER_PASS_TILE_SHADING_CREATE_INFO_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_RENDER_PASS_TILE_SHADING_CREATE_INFO_QCOM';
    end;
    VK_STRUCTURE_TYPE_PER_TILE_BEGIN_INFO_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_PER_TILE_BEGIN_INFO_QCOM';
    end;
    VK_STRUCTURE_TYPE_PER_TILE_END_INFO_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_PER_TILE_END_INFO_QCOM';
    end;
    VK_STRUCTURE_TYPE_DISPATCH_TILE_INFO_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_DISPATCH_TILE_INFO_QCOM';
    end;
    VK_STRUCTURE_TYPE_QUERY_LOW_LATENCY_SUPPORT_NV: begin
      Result := 'VK_STRUCTURE_TYPE_QUERY_LOW_LATENCY_SUPPORT_NV';
    end;
    VK_STRUCTURE_TYPE_EXPORT_METAL_OBJECT_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_EXPORT_METAL_OBJECT_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_EXPORT_METAL_OBJECTS_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_EXPORT_METAL_OBJECTS_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_EXPORT_METAL_DEVICE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_EXPORT_METAL_DEVICE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_EXPORT_METAL_COMMAND_QUEUE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_EXPORT_METAL_COMMAND_QUEUE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_EXPORT_METAL_BUFFER_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_EXPORT_METAL_BUFFER_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_IMPORT_METAL_BUFFER_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_IMPORT_METAL_BUFFER_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_EXPORT_METAL_TEXTURE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_EXPORT_METAL_TEXTURE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_IMPORT_METAL_TEXTURE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_IMPORT_METAL_TEXTURE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_EXPORT_METAL_IO_SURFACE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_EXPORT_METAL_IO_SURFACE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_IMPORT_METAL_IO_SURFACE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_IMPORT_METAL_IO_SURFACE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_EXPORT_METAL_SHARED_EVENT_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_EXPORT_METAL_SHARED_EVENT_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_IMPORT_METAL_SHARED_EVENT_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_IMPORT_METAL_SHARED_EVENT_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_DENSITY_MAP_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_DENSITY_MAP_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_DESCRIPTOR_ADDRESS_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DESCRIPTOR_ADDRESS_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_DESCRIPTOR_GET_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DESCRIPTOR_GET_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_BUFFER_CAPTURE_DESCRIPTOR_DATA_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_BUFFER_CAPTURE_DESCRIPTOR_DATA_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_IMAGE_CAPTURE_DESCRIPTOR_DATA_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_CAPTURE_DESCRIPTOR_DATA_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_IMAGE_VIEW_CAPTURE_DESCRIPTOR_DATA_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_VIEW_CAPTURE_DESCRIPTOR_DATA_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_SAMPLER_CAPTURE_DESCRIPTOR_DATA_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_SAMPLER_CAPTURE_DESCRIPTOR_DATA_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_OPAQUE_CAPTURE_DESCRIPTOR_DATA_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_OPAQUE_CAPTURE_DESCRIPTOR_DATA_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_DESCRIPTOR_BUFFER_BINDING_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DESCRIPTOR_BUFFER_BINDING_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_DESCRIPTOR_BUFFER_BINDING_PUSH_DESCRIPTOR_BUFFER_HANDLE_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DESCRIPTOR_BUFFER_BINDING_PUSH_DESCRIPTOR_BUFFER_HANDLE_EXT';
    end;
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CAPTURE_DESCRIPTOR_DATA_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CAPTURE_DESCRIPTOR_DATA_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GRAPHICS_PIPELINE_LIBRARY_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GRAPHICS_PIPELINE_LIBRARY_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GRAPHICS_PIPELINE_LIBRARY_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GRAPHICS_PIPELINE_LIBRARY_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_LIBRARY_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_LIBRARY_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_EARLY_AND_LATE_FRAGMENT_TESTS_FEATURES_AMD: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_EARLY_AND_LATE_FRAGMENT_TESTS_FEATURES_AMD';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_PROPERTIES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_PROPERTIES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_ENUM_STATE_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_ENUM_STATE_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_MOTION_TRIANGLES_DATA_NV: begin
      Result := 'VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_MOTION_TRIANGLES_DATA_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_MOTION_BLUR_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_MOTION_BLUR_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MOTION_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MOTION_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_2_PLANE_444_FORMATS_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_2_PLANE_444_FORMATS_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_COPY_COMMAND_TRANSFORM_INFO_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_COPY_COMMAND_TRANSFORM_INFO_QCOM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_COMPRESSION_CONTROL_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_COMPRESSION_CONTROL_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_IMAGE_COMPRESSION_CONTROL_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_COMPRESSION_CONTROL_EXT';
    end;
    VK_STRUCTURE_TYPE_IMAGE_COMPRESSION_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_COMPRESSION_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ATTACHMENT_FEEDBACK_LOOP_LAYOUT_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ATTACHMENT_FEEDBACK_LOOP_LAYOUT_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_4444_FORMATS_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_4444_FORMATS_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FAULT_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FAULT_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_DEVICE_FAULT_COUNTS_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_FAULT_COUNTS_EXT';
    end;
    VK_STRUCTURE_TYPE_DEVICE_FAULT_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_FAULT_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RGBA10X6_FORMATS_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RGBA10X6_FORMATS_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_DIRECTFB_SURFACE_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DIRECTFB_SURFACE_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_INPUT_DYNAMIC_STATE_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_INPUT_DYNAMIC_STATE_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_VERTEX_INPUT_BINDING_DESCRIPTION_2_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_VERTEX_INPUT_BINDING_DESCRIPTION_2_EXT';
    end;
    VK_STRUCTURE_TYPE_VERTEX_INPUT_ATTRIBUTE_DESCRIPTION_2_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_VERTEX_INPUT_ATTRIBUTE_DESCRIPTION_2_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRM_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRM_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ADDRESS_BINDING_REPORT_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ADDRESS_BINDING_REPORT_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_DEVICE_ADDRESS_BINDING_CALLBACK_DATA_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_ADDRESS_BINDING_CALLBACK_DATA_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_CONTROL_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_CONTROL_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_DEPTH_CLIP_CONTROL_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_DEPTH_CLIP_CONTROL_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIMITIVE_TOPOLOGY_LIST_RESTART_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIMITIVE_TOPOLOGY_LIST_RESTART_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_IMPORT_MEMORY_ZIRCON_HANDLE_INFO_FUCHSIA: begin
      Result := 'VK_STRUCTURE_TYPE_IMPORT_MEMORY_ZIRCON_HANDLE_INFO_FUCHSIA';
    end;
    VK_STRUCTURE_TYPE_MEMORY_ZIRCON_HANDLE_PROPERTIES_FUCHSIA: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_ZIRCON_HANDLE_PROPERTIES_FUCHSIA';
    end;
    VK_STRUCTURE_TYPE_MEMORY_GET_ZIRCON_HANDLE_INFO_FUCHSIA: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_GET_ZIRCON_HANDLE_INFO_FUCHSIA';
    end;
    VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_ZIRCON_HANDLE_INFO_FUCHSIA: begin
      Result := 'VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_ZIRCON_HANDLE_INFO_FUCHSIA';
    end;
    VK_STRUCTURE_TYPE_SEMAPHORE_GET_ZIRCON_HANDLE_INFO_FUCHSIA: begin
      Result := 'VK_STRUCTURE_TYPE_SEMAPHORE_GET_ZIRCON_HANDLE_INFO_FUCHSIA';
    end;
    VK_STRUCTURE_TYPE_BUFFER_COLLECTION_CREATE_INFO_FUCHSIA: begin
      Result := 'VK_STRUCTURE_TYPE_BUFFER_COLLECTION_CREATE_INFO_FUCHSIA';
    end;
    VK_STRUCTURE_TYPE_IMPORT_MEMORY_BUFFER_COLLECTION_FUCHSIA: begin
      Result := 'VK_STRUCTURE_TYPE_IMPORT_MEMORY_BUFFER_COLLECTION_FUCHSIA';
    end;
    VK_STRUCTURE_TYPE_BUFFER_COLLECTION_IMAGE_CREATE_INFO_FUCHSIA: begin
      Result := 'VK_STRUCTURE_TYPE_BUFFER_COLLECTION_IMAGE_CREATE_INFO_FUCHSIA';
    end;
    VK_STRUCTURE_TYPE_BUFFER_COLLECTION_PROPERTIES_FUCHSIA: begin
      Result := 'VK_STRUCTURE_TYPE_BUFFER_COLLECTION_PROPERTIES_FUCHSIA';
    end;
    VK_STRUCTURE_TYPE_BUFFER_CONSTRAINTS_INFO_FUCHSIA: begin
      Result := 'VK_STRUCTURE_TYPE_BUFFER_CONSTRAINTS_INFO_FUCHSIA';
    end;
    VK_STRUCTURE_TYPE_BUFFER_COLLECTION_BUFFER_CREATE_INFO_FUCHSIA: begin
      Result := 'VK_STRUCTURE_TYPE_BUFFER_COLLECTION_BUFFER_CREATE_INFO_FUCHSIA';
    end;
    VK_STRUCTURE_TYPE_IMAGE_CONSTRAINTS_INFO_FUCHSIA: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_CONSTRAINTS_INFO_FUCHSIA';
    end;
    VK_STRUCTURE_TYPE_IMAGE_FORMAT_CONSTRAINTS_INFO_FUCHSIA: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_FORMAT_CONSTRAINTS_INFO_FUCHSIA';
    end;
    VK_STRUCTURE_TYPE_SYSMEM_COLOR_SPACE_FUCHSIA: begin
      Result := 'VK_STRUCTURE_TYPE_SYSMEM_COLOR_SPACE_FUCHSIA';
    end;
    VK_STRUCTURE_TYPE_BUFFER_COLLECTION_CONSTRAINTS_INFO_FUCHSIA: begin
      Result := 'VK_STRUCTURE_TYPE_BUFFER_COLLECTION_CONSTRAINTS_INFO_FUCHSIA';
    end;
    VK_STRUCTURE_TYPE_SUBPASS_SHADING_PIPELINE_CREATE_INFO_HUAWEI: begin
      Result := 'VK_STRUCTURE_TYPE_SUBPASS_SHADING_PIPELINE_CREATE_INFO_HUAWEI';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBPASS_SHADING_FEATURES_HUAWEI: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBPASS_SHADING_FEATURES_HUAWEI';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBPASS_SHADING_PROPERTIES_HUAWEI: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBPASS_SHADING_PROPERTIES_HUAWEI';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INVOCATION_MASK_FEATURES_HUAWEI: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INVOCATION_MASK_FEATURES_HUAWEI';
    end;
    VK_STRUCTURE_TYPE_MEMORY_GET_REMOTE_ADDRESS_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_GET_REMOTE_ADDRESS_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_RDMA_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_RDMA_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_PROPERTIES_IDENTIFIER_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_PROPERTIES_IDENTIFIER_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_PROPERTIES_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_PROPERTIES_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAME_BOUNDARY_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAME_BOUNDARY_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_FRAME_BOUNDARY_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_FRAME_BOUNDARY_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_SUBPASS_RESOLVE_PERFORMANCE_QUERY_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_SUBPASS_RESOLVE_PERFORMANCE_QUERY_EXT';
    end;
    VK_STRUCTURE_TYPE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_2_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_2_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_SCREEN_SURFACE_CREATE_INFO_QNX: begin
      Result := 'VK_STRUCTURE_TYPE_SCREEN_SURFACE_CREATE_INFO_QNX';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COLOR_WRITE_ENABLE_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COLOR_WRITE_ENABLE_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_COLOR_WRITE_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_COLOR_WRITE_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIMITIVES_GENERATED_QUERY_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIMITIVES_GENERATED_QUERY_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_MAINTENANCE_1_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_MAINTENANCE_1_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_UNTYPED_POINTERS_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_UNTYPED_POINTERS_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VIDEO_ENCODE_RGB_CONVERSION_FEATURES_VALVE: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VIDEO_ENCODE_RGB_CONVERSION_FEATURES_VALVE';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_RGB_CONVERSION_CAPABILITIES_VALVE: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_RGB_CONVERSION_CAPABILITIES_VALVE';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_PROFILE_RGB_CONVERSION_INFO_VALVE: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_PROFILE_RGB_CONVERSION_INFO_VALVE';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_SESSION_RGB_CONVERSION_CREATE_INFO_VALVE: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_SESSION_RGB_CONVERSION_CREATE_INFO_VALVE';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_MIN_LOD_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_MIN_LOD_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_IMAGE_VIEW_MIN_LOD_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_VIEW_MIN_LOD_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTI_DRAW_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTI_DRAW_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTI_DRAW_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTI_DRAW_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_2D_VIEW_OF_3D_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_2D_VIEW_OF_3D_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TILE_IMAGE_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TILE_IMAGE_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TILE_IMAGE_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TILE_IMAGE_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_MICROMAP_BUILD_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_MICROMAP_BUILD_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_MICROMAP_VERSION_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_MICROMAP_VERSION_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_COPY_MICROMAP_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_COPY_MICROMAP_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_COPY_MICROMAP_TO_MEMORY_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_COPY_MICROMAP_TO_MEMORY_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_COPY_MEMORY_TO_MICROMAP_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_COPY_MEMORY_TO_MICROMAP_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_OPACITY_MICROMAP_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_OPACITY_MICROMAP_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_OPACITY_MICROMAP_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_OPACITY_MICROMAP_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_MICROMAP_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_MICROMAP_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_MICROMAP_BUILD_SIZES_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_MICROMAP_BUILD_SIZES_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_TRIANGLES_OPACITY_MICROMAP_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_TRIANGLES_OPACITY_MICROMAP_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISPLACEMENT_MICROMAP_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISPLACEMENT_MICROMAP_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISPLACEMENT_MICROMAP_PROPERTIES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISPLACEMENT_MICROMAP_PROPERTIES_NV';
    end;
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_TRIANGLES_DISPLACEMENT_MICROMAP_NV: begin
      Result := 'VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_TRIANGLES_DISPLACEMENT_MICROMAP_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CLUSTER_CULLING_SHADER_FEATURES_HUAWEI: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CLUSTER_CULLING_SHADER_FEATURES_HUAWEI';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CLUSTER_CULLING_SHADER_PROPERTIES_HUAWEI: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CLUSTER_CULLING_SHADER_PROPERTIES_HUAWEI';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CLUSTER_CULLING_SHADER_VRS_FEATURES_HUAWEI: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CLUSTER_CULLING_SHADER_VRS_FEATURES_HUAWEI';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BORDER_COLOR_SWIZZLE_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BORDER_COLOR_SWIZZLE_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_SAMPLER_BORDER_COLOR_COMPONENT_MAPPING_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_SAMPLER_BORDER_COLOR_COMPONENT_MAPPING_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PAGEABLE_DEVICE_LOCAL_MEMORY_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PAGEABLE_DEVICE_LOCAL_MEMORY_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_ARM';
    end;
    VK_STRUCTURE_TYPE_DEVICE_QUEUE_SHADER_CORE_CONTROL_CREATE_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_QUEUE_SHADER_CORE_CONTROL_CREATE_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_FEATURES_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_FEATURES_ARM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_PROPERTIES_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_PROPERTIES_ARM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_SLICED_VIEW_OF_3D_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_SLICED_VIEW_OF_3D_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_IMAGE_VIEW_SLICED_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_VIEW_SLICED_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_SET_HOST_MAPPING_FEATURES_VALVE: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_SET_HOST_MAPPING_FEATURES_VALVE';
    end;
    VK_STRUCTURE_TYPE_DESCRIPTOR_SET_BINDING_REFERENCE_VALVE: begin
      Result := 'VK_STRUCTURE_TYPE_DESCRIPTOR_SET_BINDING_REFERENCE_VALVE';
    end;
    VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_HOST_MAPPING_INFO_VALVE: begin
      Result := 'VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_HOST_MAPPING_INFO_VALVE';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_NON_SEAMLESS_CUBE_MAP_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_NON_SEAMLESS_CUBE_MAP_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RENDER_PASS_STRIPED_FEATURES_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RENDER_PASS_STRIPED_FEATURES_ARM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RENDER_PASS_STRIPED_PROPERTIES_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RENDER_PASS_STRIPED_PROPERTIES_ARM';
    end;
    VK_STRUCTURE_TYPE_RENDER_PASS_STRIPE_BEGIN_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_RENDER_PASS_STRIPE_BEGIN_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_RENDER_PASS_STRIPE_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_RENDER_PASS_STRIPE_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_RENDER_PASS_STRIPE_SUBMIT_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_RENDER_PASS_STRIPE_SUBMIT_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_COMPUTE_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_COMPUTE_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_INDIRECT_BUFFER_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_INDIRECT_BUFFER_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_INDIRECT_DEVICE_ADDRESS_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_INDIRECT_DEVICE_ADDRESS_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_LINEAR_SWEPT_SPHERES_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_LINEAR_SWEPT_SPHERES_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_LINEAR_SWEPT_SPHERES_DATA_NV: begin
      Result := 'VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_LINEAR_SWEPT_SPHERES_DATA_NV';
    end;
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_SPHERES_DATA_NV: begin
      Result := 'VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_SPHERES_DATA_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LINEAR_COLOR_ATTACHMENT_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LINEAR_COLOR_ATTACHMENT_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MAXIMAL_RECONVERGENCE_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MAXIMAL_RECONVERGENCE_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_COMPRESSION_CONTROL_SWAPCHAIN_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_COMPRESSION_CONTROL_SWAPCHAIN_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_FEATURES_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_FEATURES_QCOM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_PROPERTIES_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_PROPERTIES_QCOM';
    end;
    VK_STRUCTURE_TYPE_IMAGE_VIEW_SAMPLE_WEIGHT_CREATE_INFO_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_VIEW_SAMPLE_WEIGHT_CREATE_INFO_QCOM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_NESTED_COMMAND_BUFFER_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_NESTED_COMMAND_BUFFER_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_NESTED_COMMAND_BUFFER_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_NESTED_COMMAND_BUFFER_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_NATIVE_BUFFER_USAGE_OHOS: begin
      Result := 'VK_STRUCTURE_TYPE_NATIVE_BUFFER_USAGE_OHOS';
    end;
    VK_STRUCTURE_TYPE_NATIVE_BUFFER_PROPERTIES_OHOS: begin
      Result := 'VK_STRUCTURE_TYPE_NATIVE_BUFFER_PROPERTIES_OHOS';
    end;
    VK_STRUCTURE_TYPE_NATIVE_BUFFER_FORMAT_PROPERTIES_OHOS: begin
      Result := 'VK_STRUCTURE_TYPE_NATIVE_BUFFER_FORMAT_PROPERTIES_OHOS';
    end;
    VK_STRUCTURE_TYPE_IMPORT_NATIVE_BUFFER_INFO_OHOS: begin
      Result := 'VK_STRUCTURE_TYPE_IMPORT_NATIVE_BUFFER_INFO_OHOS';
    end;
    VK_STRUCTURE_TYPE_MEMORY_GET_NATIVE_BUFFER_INFO_OHOS: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_GET_NATIVE_BUFFER_INFO_OHOS';
    end;
    VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_OHOS: begin
      Result := 'VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_OHOS';
    end;
    VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_3_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_3_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_3_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_3_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBPASS_MERGE_FEEDBACK_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBPASS_MERGE_FEEDBACK_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_RENDER_PASS_CREATION_CONTROL_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_RENDER_PASS_CREATION_CONTROL_EXT';
    end;
    VK_STRUCTURE_TYPE_RENDER_PASS_CREATION_FEEDBACK_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_RENDER_PASS_CREATION_FEEDBACK_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_RENDER_PASS_SUBPASS_FEEDBACK_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_RENDER_PASS_SUBPASS_FEEDBACK_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_DIRECT_DRIVER_LOADING_INFO_LUNARG: begin
      Result := 'VK_STRUCTURE_TYPE_DIRECT_DRIVER_LOADING_INFO_LUNARG';
    end;
    VK_STRUCTURE_TYPE_DIRECT_DRIVER_LOADING_LIST_LUNARG: begin
      Result := 'VK_STRUCTURE_TYPE_DIRECT_DRIVER_LOADING_LIST_LUNARG';
    end;
    VK_STRUCTURE_TYPE_TENSOR_CREATE_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_TENSOR_CREATE_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_TENSOR_VIEW_CREATE_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_TENSOR_VIEW_CREATE_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_BIND_TENSOR_MEMORY_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_BIND_TENSOR_MEMORY_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_TENSOR_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_TENSOR_ARM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TENSOR_PROPERTIES_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TENSOR_PROPERTIES_ARM';
    end;
    VK_STRUCTURE_TYPE_TENSOR_FORMAT_PROPERTIES_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_TENSOR_FORMAT_PROPERTIES_ARM';
    end;
    VK_STRUCTURE_TYPE_TENSOR_DESCRIPTION_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_TENSOR_DESCRIPTION_ARM';
    end;
    VK_STRUCTURE_TYPE_TENSOR_MEMORY_REQUIREMENTS_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_TENSOR_MEMORY_REQUIREMENTS_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_TENSOR_MEMORY_BARRIER_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_TENSOR_MEMORY_BARRIER_ARM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TENSOR_FEATURES_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TENSOR_FEATURES_ARM';
    end;
    VK_STRUCTURE_TYPE_DEVICE_TENSOR_MEMORY_REQUIREMENTS_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_TENSOR_MEMORY_REQUIREMENTS_ARM';
    end;
    VK_STRUCTURE_TYPE_COPY_TENSOR_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_COPY_TENSOR_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_TENSOR_COPY_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_TENSOR_COPY_ARM';
    end;
    VK_STRUCTURE_TYPE_TENSOR_DEPENDENCY_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_TENSOR_DEPENDENCY_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_TENSOR_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_TENSOR_ARM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_TENSOR_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_TENSOR_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_EXTERNAL_TENSOR_PROPERTIES_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_EXTERNAL_TENSOR_PROPERTIES_ARM';
    end;
    VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_TENSOR_CREATE_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_TENSOR_CREATE_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_TENSOR_FEATURES_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_TENSOR_FEATURES_ARM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_TENSOR_PROPERTIES_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_TENSOR_PROPERTIES_ARM';
    end;
    VK_STRUCTURE_TYPE_DESCRIPTOR_GET_TENSOR_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_DESCRIPTOR_GET_TENSOR_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_TENSOR_CAPTURE_DESCRIPTOR_DATA_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_TENSOR_CAPTURE_DESCRIPTOR_DATA_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_TENSOR_VIEW_CAPTURE_DESCRIPTOR_DATA_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_TENSOR_VIEW_CAPTURE_DESCRIPTOR_DATA_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_FRAME_BOUNDARY_TENSORS_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_FRAME_BOUNDARY_TENSORS_ARM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MODULE_IDENTIFIER_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MODULE_IDENTIFIER_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MODULE_IDENTIFIER_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MODULE_IDENTIFIER_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_MODULE_IDENTIFIER_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_MODULE_IDENTIFIER_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_SHADER_MODULE_IDENTIFIER_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_SHADER_MODULE_IDENTIFIER_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_OPTICAL_FLOW_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_OPTICAL_FLOW_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_OPTICAL_FLOW_PROPERTIES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_OPTICAL_FLOW_PROPERTIES_NV';
    end;
    VK_STRUCTURE_TYPE_OPTICAL_FLOW_IMAGE_FORMAT_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_OPTICAL_FLOW_IMAGE_FORMAT_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_OPTICAL_FLOW_IMAGE_FORMAT_PROPERTIES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_OPTICAL_FLOW_IMAGE_FORMAT_PROPERTIES_NV';
    end;
    VK_STRUCTURE_TYPE_OPTICAL_FLOW_SESSION_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_OPTICAL_FLOW_SESSION_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_OPTICAL_FLOW_EXECUTE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_OPTICAL_FLOW_EXECUTE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_OPTICAL_FLOW_SESSION_CREATE_PRIVATE_DATA_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_OPTICAL_FLOW_SESSION_CREATE_PRIVATE_DATA_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LEGACY_DITHERING_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LEGACY_DITHERING_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FORMAT_RESOLVE_FEATURES_ANDROID: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FORMAT_RESOLVE_FEATURES_ANDROID';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FORMAT_RESOLVE_PROPERTIES_ANDROID: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FORMAT_RESOLVE_PROPERTIES_ANDROID';
    end;
    VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_RESOLVE_PROPERTIES_ANDROID: begin
      Result := 'VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_RESOLVE_PROPERTIES_ANDROID';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ANTI_LAG_FEATURES_AMD: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ANTI_LAG_FEATURES_AMD';
    end;
    VK_STRUCTURE_TYPE_ANTI_LAG_DATA_AMD: begin
      Result := 'VK_STRUCTURE_TYPE_ANTI_LAG_DATA_AMD';
    end;
    VK_STRUCTURE_TYPE_ANTI_LAG_PRESENTATION_INFO_AMD: begin
      Result := 'VK_STRUCTURE_TYPE_ANTI_LAG_PRESENTATION_INFO_AMD';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DENSE_GEOMETRY_FORMAT_FEATURES_AMDX: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DENSE_GEOMETRY_FORMAT_FEATURES_AMDX';
    end;
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DENSE_GEOMETRY_FORMAT_TRIANGLES_DATA_AMDX: begin
      Result := 'VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DENSE_GEOMETRY_FORMAT_TRIANGLES_DATA_AMDX';
    end;
    VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_PRESENT_ID_2_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_PRESENT_ID_2_KHR';
    end;
    VK_STRUCTURE_TYPE_PRESENT_ID_2_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PRESENT_ID_2_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_ID_2_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_ID_2_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_PRESENT_WAIT_2_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_PRESENT_WAIT_2_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_WAIT_2_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_WAIT_2_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PRESENT_WAIT_2_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PRESENT_WAIT_2_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_POSITION_FETCH_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_POSITION_FETCH_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_OBJECT_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_OBJECT_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_OBJECT_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_OBJECT_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_SHADER_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_SHADER_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_BINARY_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_BINARY_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_BINARY_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_BINARY_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_BINARY_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_BINARY_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_BINARY_KEY_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_BINARY_KEY_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_BINARY_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_BINARY_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_RELEASE_CAPTURED_PIPELINE_DATA_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_RELEASE_CAPTURED_PIPELINE_DATA_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_BINARY_DATA_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_BINARY_DATA_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_DEVICE_PIPELINE_BINARY_INTERNAL_CACHE_CONTROL_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_DEVICE_PIPELINE_BINARY_INTERNAL_CACHE_CONTROL_KHR';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_BINARY_HANDLES_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_BINARY_HANDLES_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_PROPERTIES_FEATURES_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_PROPERTIES_FEATURES_QCOM';
    end;
    VK_STRUCTURE_TYPE_TILE_PROPERTIES_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_TILE_PROPERTIES_QCOM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_AMIGO_PROFILING_FEATURES_SEC: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_AMIGO_PROFILING_FEATURES_SEC';
    end;
    VK_STRUCTURE_TYPE_AMIGO_PROFILING_SUBMIT_INFO_SEC: begin
      Result := 'VK_STRUCTURE_TYPE_AMIGO_PROFILING_SUBMIT_INFO_SEC';
    end;
    VK_STRUCTURE_TYPE_SURFACE_PRESENT_MODE_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_SURFACE_PRESENT_MODE_KHR';
    end;
    VK_STRUCTURE_TYPE_SURFACE_PRESENT_SCALING_CAPABILITIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_SURFACE_PRESENT_SCALING_CAPABILITIES_KHR';
    end;
    VK_STRUCTURE_TYPE_SURFACE_PRESENT_MODE_COMPATIBILITY_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_SURFACE_PRESENT_MODE_COMPATIBILITY_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SWAPCHAIN_MAINTENANCE_1_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SWAPCHAIN_MAINTENANCE_1_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODES_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODES_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_SWAPCHAIN_PRESENT_SCALING_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_SWAPCHAIN_PRESENT_SCALING_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_RELEASE_SWAPCHAIN_IMAGES_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_RELEASE_SWAPCHAIN_IMAGES_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_VIEWPORTS_FEATURES_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_VIEWPORTS_FEATURES_QCOM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_PROPERTIES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_PROPERTIES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_VECTOR_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_VECTOR_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_VECTOR_PROPERTIES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_VECTOR_PROPERTIES_NV';
    end;
    VK_STRUCTURE_TYPE_COOPERATIVE_VECTOR_PROPERTIES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_COOPERATIVE_VECTOR_PROPERTIES_NV';
    end;
    VK_STRUCTURE_TYPE_CONVERT_COOPERATIVE_VECTOR_MATRIX_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_CONVERT_COOPERATIVE_VECTOR_MATRIX_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_SPARSE_ADDRESS_SPACE_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_SPARSE_ADDRESS_SPACE_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_SPARSE_ADDRESS_SPACE_PROPERTIES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_SPARSE_ADDRESS_SPACE_PROPERTIES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MUTABLE_DESCRIPTOR_TYPE_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MUTABLE_DESCRIPTOR_TYPE_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LEGACY_VERTEX_ATTRIBUTES_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LEGACY_VERTEX_ATTRIBUTES_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LEGACY_VERTEX_ATTRIBUTES_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LEGACY_VERTEX_ATTRIBUTES_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_LAYER_SETTINGS_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_LAYER_SETTINGS_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_BUILTINS_FEATURES_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_BUILTINS_FEATURES_ARM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_BUILTINS_PROPERTIES_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_BUILTINS_PROPERTIES_ARM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_LIBRARY_GROUP_HANDLES_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_LIBRARY_GROUP_HANDLES_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_UNUSED_ATTACHMENTS_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_UNUSED_ATTACHMENTS_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_LATENCY_SLEEP_MODE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_LATENCY_SLEEP_MODE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_LATENCY_SLEEP_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_LATENCY_SLEEP_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_SET_LATENCY_MARKER_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_SET_LATENCY_MARKER_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_GET_LATENCY_MARKER_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_GET_LATENCY_MARKER_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_LATENCY_TIMINGS_FRAME_REPORT_NV: begin
      Result := 'VK_STRUCTURE_TYPE_LATENCY_TIMINGS_FRAME_REPORT_NV';
    end;
    VK_STRUCTURE_TYPE_LATENCY_SUBMISSION_PRESENT_ID_NV: begin
      Result := 'VK_STRUCTURE_TYPE_LATENCY_SUBMISSION_PRESENT_ID_NV';
    end;
    VK_STRUCTURE_TYPE_OUT_OF_BAND_QUEUE_TYPE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_OUT_OF_BAND_QUEUE_TYPE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_SWAPCHAIN_LATENCY_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_SWAPCHAIN_LATENCY_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_LATENCY_SURFACE_CAPABILITIES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_LATENCY_SURFACE_CAPABILITIES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_CREATE_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_CREATE_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_CREATE_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_CREATE_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_RESOURCE_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_RESOURCE_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_CONSTANT_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_CONSTANT_ARM';
    end;
    VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_MEMORY_REQUIREMENTS_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_MEMORY_REQUIREMENTS_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_BIND_DATA_GRAPH_PIPELINE_SESSION_MEMORY_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_BIND_DATA_GRAPH_PIPELINE_SESSION_MEMORY_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DATA_GRAPH_FEATURES_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DATA_GRAPH_FEATURES_ARM';
    end;
    VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SHADER_MODULE_CREATE_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SHADER_MODULE_CREATE_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_PROPERTY_QUERY_RESULT_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_PROPERTY_QUERY_RESULT_ARM';
    end;
    VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_COMPILER_CONTROL_CREATE_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_COMPILER_CONTROL_CREATE_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_REQUIREMENTS_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_REQUIREMENTS_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_REQUIREMENT_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_REQUIREMENT_ARM';
    end;
    VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_IDENTIFIER_CREATE_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_IDENTIFIER_CREATE_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_DISPATCH_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_DISPATCH_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_DATA_GRAPH_PROCESSING_ENGINE_CREATE_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_DATA_GRAPH_PROCESSING_ENGINE_CREATE_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_QUEUE_FAMILY_DATA_GRAPH_PROCESSING_ENGINE_PROPERTIES_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_QUEUE_FAMILY_DATA_GRAPH_PROCESSING_ENGINE_PROPERTIES_ARM';
    end;
    VK_STRUCTURE_TYPE_QUEUE_FAMILY_DATA_GRAPH_PROPERTIES_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_QUEUE_FAMILY_DATA_GRAPH_PROPERTIES_ARM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_QUEUE_FAMILY_DATA_GRAPH_PROCESSING_ENGINE_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_QUEUE_FAMILY_DATA_GRAPH_PROCESSING_ENGINE_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_CONSTANT_TENSOR_SEMI_STRUCTURED_SPARSITY_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_CONSTANT_TENSOR_SEMI_STRUCTURED_SPARSITY_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_RENDER_AREAS_FEATURES_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_RENDER_AREAS_FEATURES_QCOM';
    end;
    VK_STRUCTURE_TYPE_MULTIVIEW_PER_VIEW_RENDER_AREAS_RENDER_PASS_BEGIN_INFO_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_MULTIVIEW_PER_VIEW_RENDER_AREAS_RENDER_PASS_BEGIN_INFO_QCOM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_AV1_CAPABILITIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_AV1_CAPABILITIES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_AV1_PICTURE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_AV1_PICTURE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_AV1_PROFILE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_AV1_PROFILE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_AV1_SESSION_PARAMETERS_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_AV1_SESSION_PARAMETERS_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_AV1_DPB_SLOT_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_AV1_DPB_SLOT_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_CAPABILITIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_CAPABILITIES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_SESSION_PARAMETERS_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_SESSION_PARAMETERS_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_PICTURE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_PICTURE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_DPB_SLOT_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_DPB_SLOT_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VIDEO_ENCODE_AV1_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VIDEO_ENCODE_AV1_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_PROFILE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_PROFILE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_RATE_CONTROL_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_RATE_CONTROL_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_RATE_CONTROL_LAYER_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_RATE_CONTROL_LAYER_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_QUALITY_LEVEL_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_QUALITY_LEVEL_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_SESSION_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_SESSION_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_GOP_REMAINING_FRAME_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_GOP_REMAINING_FRAME_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VIDEO_DECODE_VP9_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VIDEO_DECODE_VP9_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_VP9_CAPABILITIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_VP9_CAPABILITIES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_VP9_PICTURE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_VP9_PICTURE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_VP9_PROFILE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_VP9_PROFILE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VIDEO_MAINTENANCE_1_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VIDEO_MAINTENANCE_1_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_INLINE_QUERY_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_INLINE_QUERY_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PER_STAGE_DESCRIPTOR_SET_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PER_STAGE_DESCRIPTOR_SET_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_2_FEATURES_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_2_FEATURES_QCOM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_2_PROPERTIES_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_2_PROPERTIES_QCOM';
    end;
    VK_STRUCTURE_TYPE_SAMPLER_BLOCK_MATCH_WINDOW_CREATE_INFO_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_SAMPLER_BLOCK_MATCH_WINDOW_CREATE_INFO_QCOM';
    end;
    VK_STRUCTURE_TYPE_SAMPLER_CUBIC_WEIGHTS_CREATE_INFO_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_SAMPLER_CUBIC_WEIGHTS_CREATE_INFO_QCOM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUBIC_WEIGHTS_FEATURES_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUBIC_WEIGHTS_FEATURES_QCOM';
    end;
    VK_STRUCTURE_TYPE_BLIT_IMAGE_CUBIC_WEIGHTS_INFO_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_BLIT_IMAGE_CUBIC_WEIGHTS_INFO_QCOM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_DEGAMMA_FEATURES_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_DEGAMMA_FEATURES_QCOM';
    end;
    VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_YCBCR_DEGAMMA_CREATE_INFO_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_YCBCR_DEGAMMA_CREATE_INFO_QCOM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUBIC_CLAMP_FEATURES_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUBIC_CLAMP_FEATURES_QCOM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ATTACHMENT_FEEDBACK_LOOP_DYNAMIC_STATE_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ATTACHMENT_FEEDBACK_LOOP_DYNAMIC_STATE_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFIED_IMAGE_LAYOUTS_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFIED_IMAGE_LAYOUTS_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_ATTACHMENT_FEEDBACK_LOOP_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_ATTACHMENT_FEEDBACK_LOOP_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_SCREEN_BUFFER_PROPERTIES_QNX: begin
      Result := 'VK_STRUCTURE_TYPE_SCREEN_BUFFER_PROPERTIES_QNX';
    end;
    VK_STRUCTURE_TYPE_SCREEN_BUFFER_FORMAT_PROPERTIES_QNX: begin
      Result := 'VK_STRUCTURE_TYPE_SCREEN_BUFFER_FORMAT_PROPERTIES_QNX';
    end;
    VK_STRUCTURE_TYPE_IMPORT_SCREEN_BUFFER_INFO_QNX: begin
      Result := 'VK_STRUCTURE_TYPE_IMPORT_SCREEN_BUFFER_INFO_QNX';
    end;
    VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_QNX: begin
      Result := 'VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_QNX';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_SCREEN_BUFFER_FEATURES_QNX: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_SCREEN_BUFFER_FEATURES_QNX';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_DRIVER_PROPERTIES_MSFT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_DRIVER_PROPERTIES_MSFT';
    end;
    VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_SET_DESCRIPTOR_BUFFER_OFFSETS_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_SET_DESCRIPTOR_BUFFER_OFFSETS_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_BIND_DESCRIPTOR_BUFFER_EMBEDDED_SAMPLERS_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_BIND_DESCRIPTOR_BUFFER_EMBEDDED_SAMPLERS_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_POOL_OVERALLOCATION_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_POOL_OVERALLOCATION_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_MEMORY_HEAP_FEATURES_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_MEMORY_HEAP_FEATURES_QCOM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_MEMORY_HEAP_PROPERTIES_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_MEMORY_HEAP_PROPERTIES_QCOM';
    end;
    VK_STRUCTURE_TYPE_TILE_MEMORY_REQUIREMENTS_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_TILE_MEMORY_REQUIREMENTS_QCOM';
    end;
    VK_STRUCTURE_TYPE_TILE_MEMORY_BIND_INFO_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_TILE_MEMORY_BIND_INFO_QCOM';
    end;
    VK_STRUCTURE_TYPE_TILE_MEMORY_SIZE_INFO_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_TILE_MEMORY_SIZE_INFO_QCOM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_COPY_MEMORY_INDIRECT_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_COPY_MEMORY_INDIRECT_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INDIRECT_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INDIRECT_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_DECOMPRESS_MEMORY_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_DECOMPRESS_MEMORY_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_DISPLAY_SURFACE_STEREO_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_DISPLAY_SURFACE_STEREO_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_DISPLAY_MODE_STEREO_PROPERTIES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_DISPLAY_MODE_STEREO_PROPERTIES_NV';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_INTRA_REFRESH_CAPABILITIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_INTRA_REFRESH_CAPABILITIES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_SESSION_INTRA_REFRESH_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_SESSION_INTRA_REFRESH_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_INTRA_REFRESH_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_INTRA_REFRESH_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_REFERENCE_INTRA_REFRESH_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_REFERENCE_INTRA_REFRESH_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VIDEO_ENCODE_INTRA_REFRESH_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VIDEO_ENCODE_INTRA_REFRESH_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_QUANTIZATION_MAP_CAPABILITIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_QUANTIZATION_MAP_CAPABILITIES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_FORMAT_QUANTIZATION_MAP_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_FORMAT_QUANTIZATION_MAP_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_QUANTIZATION_MAP_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_QUANTIZATION_MAP_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_QUANTIZATION_MAP_SESSION_PARAMETERS_CREATE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_QUANTIZATION_MAP_SESSION_PARAMETERS_CREATE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VIDEO_ENCODE_QUANTIZATION_MAP_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VIDEO_ENCODE_QUANTIZATION_MAP_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_QUANTIZATION_MAP_CAPABILITIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H264_QUANTIZATION_MAP_CAPABILITIES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_QUANTIZATION_MAP_CAPABILITIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_QUANTIZATION_MAP_CAPABILITIES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_FORMAT_H265_QUANTIZATION_MAP_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_FORMAT_H265_QUANTIZATION_MAP_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_QUANTIZATION_MAP_CAPABILITIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_QUANTIZATION_MAP_CAPABILITIES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_FORMAT_AV1_QUANTIZATION_MAP_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_FORMAT_AV1_QUANTIZATION_MAP_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAW_ACCESS_CHAINS_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAW_ACCESS_CHAINS_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_EXTERNAL_COMPUTE_QUEUE_DEVICE_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_EXTERNAL_COMPUTE_QUEUE_DEVICE_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_EXTERNAL_COMPUTE_QUEUE_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_EXTERNAL_COMPUTE_QUEUE_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_EXTERNAL_COMPUTE_QUEUE_DATA_PARAMS_NV: begin
      Result := 'VK_STRUCTURE_TYPE_EXTERNAL_COMPUTE_QUEUE_DATA_PARAMS_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_COMPUTE_QUEUE_PROPERTIES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_COMPUTE_QUEUE_PROPERTIES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_RELAXED_EXTENDED_INSTRUCTION_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_RELAXED_EXTENDED_INSTRUCTION_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMMAND_BUFFER_INHERITANCE_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMMAND_BUFFER_INHERITANCE_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_7_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_7_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_7_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_7_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_API_PROPERTIES_LIST_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_API_PROPERTIES_LIST_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_API_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_API_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_API_VULKAN_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_API_VULKAN_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT16_VECTOR_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT16_VECTOR_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_REPLICATED_COMPOSITES_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_REPLICATED_COMPOSITES_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT8_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT8_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_VALIDATION_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_VALIDATION_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CLUSTER_ACCELERATION_STRUCTURE_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CLUSTER_ACCELERATION_STRUCTURE_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CLUSTER_ACCELERATION_STRUCTURE_PROPERTIES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CLUSTER_ACCELERATION_STRUCTURE_PROPERTIES_NV';
    end;
    VK_STRUCTURE_TYPE_CLUSTER_ACCELERATION_STRUCTURE_CLUSTERS_BOTTOM_LEVEL_INPUT_NV: begin
      Result := 'VK_STRUCTURE_TYPE_CLUSTER_ACCELERATION_STRUCTURE_CLUSTERS_BOTTOM_LEVEL_INPUT_NV';
    end;
    VK_STRUCTURE_TYPE_CLUSTER_ACCELERATION_STRUCTURE_TRIANGLE_CLUSTER_INPUT_NV: begin
      Result := 'VK_STRUCTURE_TYPE_CLUSTER_ACCELERATION_STRUCTURE_TRIANGLE_CLUSTER_INPUT_NV';
    end;
    VK_STRUCTURE_TYPE_CLUSTER_ACCELERATION_STRUCTURE_MOVE_OBJECTS_INPUT_NV: begin
      Result := 'VK_STRUCTURE_TYPE_CLUSTER_ACCELERATION_STRUCTURE_MOVE_OBJECTS_INPUT_NV';
    end;
    VK_STRUCTURE_TYPE_CLUSTER_ACCELERATION_STRUCTURE_INPUT_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_CLUSTER_ACCELERATION_STRUCTURE_INPUT_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_CLUSTER_ACCELERATION_STRUCTURE_COMMANDS_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_CLUSTER_ACCELERATION_STRUCTURE_COMMANDS_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CLUSTER_ACCELERATION_STRUCTURE_CREATE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CLUSTER_ACCELERATION_STRUCTURE_CREATE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PARTITIONED_ACCELERATION_STRUCTURE_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PARTITIONED_ACCELERATION_STRUCTURE_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PARTITIONED_ACCELERATION_STRUCTURE_PROPERTIES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PARTITIONED_ACCELERATION_STRUCTURE_PROPERTIES_NV';
    end;
    VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_PARTITIONED_ACCELERATION_STRUCTURE_NV: begin
      Result := 'VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_PARTITIONED_ACCELERATION_STRUCTURE_NV';
    end;
    VK_STRUCTURE_TYPE_PARTITIONED_ACCELERATION_STRUCTURE_INSTANCES_INPUT_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PARTITIONED_ACCELERATION_STRUCTURE_INSTANCES_INPUT_NV';
    end;
    VK_STRUCTURE_TYPE_BUILD_PARTITIONED_ACCELERATION_STRUCTURE_INFO_NV: begin
      Result := 'VK_STRUCTURE_TYPE_BUILD_PARTITIONED_ACCELERATION_STRUCTURE_INFO_NV';
    end;
    VK_STRUCTURE_TYPE_PARTITIONED_ACCELERATION_STRUCTURE_FLAGS_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PARTITIONED_ACCELERATION_STRUCTURE_FLAGS_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_EXT';
    end;
    VK_STRUCTURE_TYPE_WRITE_INDIRECT_EXECUTION_SET_PIPELINE_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_WRITE_INDIRECT_EXECUTION_SET_PIPELINE_EXT';
    end;
    VK_STRUCTURE_TYPE_WRITE_INDIRECT_EXECUTION_SET_SHADER_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_WRITE_INDIRECT_EXECUTION_SET_SHADER_EXT';
    end;
    VK_STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_PIPELINE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_PIPELINE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_SHADER_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_SHADER_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_SHADER_LAYOUT_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_SHADER_LAYOUT_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_GENERATED_COMMANDS_PIPELINE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_GENERATED_COMMANDS_PIPELINE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_GENERATED_COMMANDS_SHADER_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_GENERATED_COMMANDS_SHADER_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_8_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_8_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_MEMORY_BARRIER_ACCESS_FLAGS_3_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_BARRIER_ACCESS_FLAGS_3_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ALIGNMENT_CONTROL_FEATURES_MESA: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ALIGNMENT_CONTROL_FEATURES_MESA';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ALIGNMENT_CONTROL_PROPERTIES_MESA: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ALIGNMENT_CONTROL_PROPERTIES_MESA';
    end;
    VK_STRUCTURE_TYPE_IMAGE_ALIGNMENT_CONTROL_CREATE_INFO_MESA: begin
      Result := 'VK_STRUCTURE_TYPE_IMAGE_ALIGNMENT_CONTROL_CREATE_INFO_MESA';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FMA_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FMA_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_CONTROL_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_CONTROL_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_DEPTH_CLAMP_CONTROL_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_DEPTH_CLAMP_CONTROL_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_9_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_9_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_9_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_9_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_QUEUE_FAMILY_OWNERSHIP_TRANSFER_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_QUEUE_FAMILY_OWNERSHIP_TRANSFER_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VIDEO_MAINTENANCE_2_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VIDEO_MAINTENANCE_2_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_H264_INLINE_SESSION_PARAMETERS_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_H264_INLINE_SESSION_PARAMETERS_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_H265_INLINE_SESSION_PARAMETERS_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_H265_INLINE_SESSION_PARAMETERS_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_VIDEO_DECODE_AV1_INLINE_SESSION_PARAMETERS_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_VIDEO_DECODE_AV1_INLINE_SESSION_PARAMETERS_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_SURFACE_CREATE_INFO_OHOS: begin
      Result := 'VK_STRUCTURE_TYPE_SURFACE_CREATE_INFO_OHOS';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HDR_VIVID_FEATURES_HUAWEI: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HDR_VIVID_FEATURES_HUAWEI';
    end;
    VK_STRUCTURE_TYPE_HDR_VIVID_DYNAMIC_METADATA_HUAWEI: begin
      Result := 'VK_STRUCTURE_TYPE_HDR_VIVID_DYNAMIC_METADATA_HUAWEI';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_2_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_2_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_FLEXIBLE_DIMENSIONS_PROPERTIES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_FLEXIBLE_DIMENSIONS_PROPERTIES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_2_PROPERTIES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_2_PROPERTIES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_OPACITY_MICROMAP_FEATURES_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_OPACITY_MICROMAP_FEATURES_ARM';
    end;
    VK_STRUCTURE_TYPE_IMPORT_MEMORY_METAL_HANDLE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_IMPORT_MEMORY_METAL_HANDLE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_MEMORY_METAL_HANDLE_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_METAL_HANDLE_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_MEMORY_GET_METAL_HANDLE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_MEMORY_GET_METAL_HANDLE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_ZERO_ONE_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_ZERO_ONE_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_COUNTERS_BY_REGION_FEATURES_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_COUNTERS_BY_REGION_FEATURES_ARM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_COUNTERS_BY_REGION_PROPERTIES_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_COUNTERS_BY_REGION_PROPERTIES_ARM';
    end;
    VK_STRUCTURE_TYPE_PERFORMANCE_COUNTER_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_PERFORMANCE_COUNTER_ARM';
    end;
    VK_STRUCTURE_TYPE_PERFORMANCE_COUNTER_DESCRIPTION_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_PERFORMANCE_COUNTER_DESCRIPTION_ARM';
    end;
    VK_STRUCTURE_TYPE_RENDER_PASS_PERFORMANCE_COUNTERS_BY_REGION_BEGIN_INFO_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_RENDER_PASS_PERFORMANCE_COUNTERS_BY_REGION_BEGIN_INFO_ARM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_ROBUSTNESS_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_ROBUSTNESS_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FORMAT_PACK_FEATURES_ARM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FORMAT_PACK_FEATURES_ARM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_LAYERED_FEATURES_VALVE: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_LAYERED_FEATURES_VALVE';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_LAYERED_PROPERTIES_VALVE: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_LAYERED_PROPERTIES_VALVE';
    end;
    VK_STRUCTURE_TYPE_PIPELINE_FRAGMENT_DENSITY_MAP_LAYERED_CREATE_INFO_VALVE: begin
      Result := 'VK_STRUCTURE_TYPE_PIPELINE_FRAGMENT_DENSITY_MAP_LAYERED_CREATE_INFO_VALVE';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_SET_PRESENT_CONFIG_NV: begin
      Result := 'VK_STRUCTURE_TYPE_SET_PRESENT_CONFIG_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_METERING_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_METERING_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_OFFSET_END_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_OFFSET_END_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_DEVICE_MEMORY_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_DEVICE_MEMORY_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_MODE_FIFO_LATEST_READY_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_MODE_FIFO_LATEST_READY_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_64_BIT_INDEXING_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_64_BIT_INDEXING_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_RESOLVE_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_RESOLVE_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_BEGIN_CUSTOM_RESOLVE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_BEGIN_CUSTOM_RESOLVE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_CUSTOM_RESOLVE_CREATE_INFO_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_CUSTOM_RESOLVE_CREATE_INFO_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DATA_GRAPH_MODEL_FEATURES_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DATA_GRAPH_MODEL_FEATURES_QCOM';
    end;
    VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_BUILTIN_MODEL_CREATE_INFO_QCOM: begin
      Result := 'VK_STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_BUILTIN_MODEL_CREATE_INFO_QCOM';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_10_FEATURES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_10_FEATURES_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_10_PROPERTIES_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_10_PROPERTIES_KHR';
    end;
    VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_FLAGS_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_FLAGS_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_RENDERING_END_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_RENDERING_END_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_RESOLVE_IMAGE_MODE_INFO_KHR: begin
      Result := 'VK_STRUCTURE_TYPE_RESOLVE_IMAGE_MODE_INFO_KHR';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_LONG_VECTOR_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_LONG_VECTOR_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_LONG_VECTOR_PROPERTIES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_LONG_VECTOR_PROPERTIES_EXT';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CACHE_INCREMENTAL_MODE_FEATURES_SEC: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CACHE_INCREMENTAL_MODE_FEATURES_SEC';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_UNIFORM_BUFFER_UNSIZED_ARRAY_FEATURES_EXT: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_UNIFORM_BUFFER_UNSIZED_ARRAY_FEATURES_EXT';
    end;
    VK_STRUCTURE_TYPE_COMPUTE_OCCUPANCY_PRIORITY_PARAMETERS_NV: begin
      Result := 'VK_STRUCTURE_TYPE_COMPUTE_OCCUPANCY_PRIORITY_PARAMETERS_NV';
    end;
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_OCCUPANCY_PRIORITY_FEATURES_NV: begin
      Result := 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_OCCUPANCY_PRIORITY_FEATURES_NV';
    end;
    VK_STRUCTURE_TYPE_MAX_ENUM: begin
      Result := 'VK_STRUCTURE_TYPE_MAX_ENUM';
    end;
  end;
end;

operator := (i: longword): boolean;
begin
  Result := i <> 0;
end;

operator := (ai: VkApplicationInfo): string;
begin
  ai.ToString;
end;

operator := (ici: VkInstanceCreateInfo): string;
begin
  Result := ici.ToString;
end;


operator := (Value: VkImageLayout): string; inline;
begin
  case Value of
    VK_IMAGE_LAYOUT_UNDEFINED: Result := 'UNDEFINED';
    VK_IMAGE_LAYOUT_GENERAL: Result := 'GENERAL';
    VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL: Result := 'COLOR_ATTACHMENT_OPTIMAL';
    VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL: Result := 'DEPTH_STENCIL_ATTACHMENT_OPTIMAL';
    VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL: Result := 'DEPTH_STENCIL_READ_ONLY_OPTIMAL';
    VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL: Result := 'SHADER_READ_ONLY_OPTIMAL';
    VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL: Result := 'TRANSFER_SRC_OPTIMAL';
    VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL: Result := 'TRANSFER_DST_OPTIMAL';
    VK_IMAGE_LAYOUT_PREINITIALIZED: Result := 'PREINITIALIZED';
    VK_IMAGE_LAYOUT_PRESENT_SRC_KHR: Result := 'PRESENT_SRC_KHR';
    else
      Result := Format('VkImageLayout(%d)', [Ord(Value)]);
  end;
end;

operator := (Value: VkPipelineStageFlagBits): string; inline;
begin
  // Restituisce una stringa compatta con i bit principali
  Result := '';
  if Value = VK_PIPELINE_STAGE_ALL_COMMANDS_BIT then
    Result := 'ALL_COMMANDS'
  else
  begin
    if Value and VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT <> 0 then Result := Result + 'TOP_OF_PIPE|';
    if Value and VK_PIPELINE_STAGE_VERTEX_SHADER_BIT <> 0 then Result := Result + 'VERTEX_SHADER|';
    if Value and VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT <> 0 then Result := Result + 'FRAGMENT_SHADER|';
    if Value and VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT <> 0 then Result := Result + 'COLOR_OUTPUT|';
    if Value and VK_PIPELINE_STAGE_TRANSFER_BIT <> 0 then Result := Result + 'TRANSFER|';
    if Value and VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT <> 0 then Result := Result + 'BOTTOM_OF_PIPE|';
    if Result <> '' then Delete(Result, Length(Result), 1);
  end;
  if Result = '' then Result := '0';
end;

operator := (Value: VkAccessFlagBits): string; inline;
begin
  Result := '';
  if Value and VK_ACCESS_INDIRECT_COMMAND_READ_BIT <> 0 then Result := Result + 'INDIRECT_READ|';
  if Value and VK_ACCESS_INDEX_READ_BIT <> 0 then Result := Result + 'INDEX_READ|';
  if Value and VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT <> 0 then Result := Result + 'VERTEX_ATTR_READ|';
  if Value and VK_ACCESS_UNIFORM_READ_BIT <> 0 then Result := Result + 'UNIFORM_READ|';
  if Value and VK_ACCESS_SHADER_READ_BIT <> 0 then Result := Result + 'SHADER_READ|';
  if Value and VK_ACCESS_SHADER_WRITE_BIT <> 0 then Result := Result + 'SHADER_WRITE|';
  if Value and VK_ACCESS_COLOR_ATTACHMENT_READ_BIT <> 0 then Result := Result + 'COLOR_READ|';
  if Value and VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT <> 0 then Result := Result + 'COLOR_WRITE|';
  if Value and VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT <> 0 then Result := Result + 'DS_READ|';
  if Value and VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT <> 0 then Result := Result + 'DS_WRITE|';
  if Value and VK_ACCESS_TRANSFER_READ_BIT <> 0 then Result := Result + 'TRANSFER_READ|';
  if Value and VK_ACCESS_TRANSFER_WRITE_BIT <> 0 then Result := Result + 'TRANSFER_WRITE|';
  if Value and VK_ACCESS_HOST_READ_BIT <> 0 then Result := Result + 'HOST_READ|';
  if Value and VK_ACCESS_HOST_WRITE_BIT <> 0 then Result := Result + 'HOST_WRITE|';
  if Value and VK_ACCESS_MEMORY_READ_BIT <> 0 then Result := Result + 'MEMORY_READ|';
  if Value and VK_ACCESS_MEMORY_WRITE_BIT <> 0 then Result := Result + 'MEMORY_WRITE|';
  if Result <> '' then Delete(Result, Length(Result), 1);
  if Result = '' then Result := '0';
end;

operator := (Value: VkSampleCountFlagBits): string; inline;
begin
  case Value of
    VK_SAMPLE_COUNT_1_BIT: Result := '1';
    VK_SAMPLE_COUNT_2_BIT: Result := '2';
    VK_SAMPLE_COUNT_4_BIT: Result := '4';
    VK_SAMPLE_COUNT_8_BIT: Result := '8';
    VK_SAMPLE_COUNT_16_BIT: Result := '16';
    VK_SAMPLE_COUNT_32_BIT: Result := '32';
    VK_SAMPLE_COUNT_64_BIT: Result := '64';
    else
      Result := Format('VkSampleCount(%d)', [Ord(Value)]);
  end;
end;

operator := (Value: VkImageUsageFlagBits): string; inline;
begin
  Result := '';
  if Value and VK_IMAGE_USAGE_TRANSFER_SRC_BIT <> 0 then Result := Result + 'TRANSFER_SRC|';
  if Value and VK_IMAGE_USAGE_TRANSFER_DST_BIT <> 0 then Result := Result + 'TRANSFER_DST|';
  if Value and VK_IMAGE_USAGE_SAMPLED_BIT <> 0 then Result := Result + 'SAMPLED|';
  if Value and VK_IMAGE_USAGE_STORAGE_BIT <> 0 then Result := Result + 'STORAGE|';
  if Value and VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT <> 0 then Result := Result + 'COLOR_ATTACHMENT|';
  if Value and VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT <> 0 then Result := Result + 'DEPTH_STENCIL|';
  if Value and VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT <> 0 then Result := Result + 'TRANSIENT|';
  if Value and VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT <> 0 then Result := Result + 'INPUT_ATTACHMENT|';
  if Result <> '' then Delete(Result, Length(Result), 1);
  if Result = '' then Result := '0';
end;

operator := (Value: VkSharingMode): string; inline;
begin
  case Value of
    VK_SHARING_MODE_EXCLUSIVE: Result := 'EXCLUSIVE';
    VK_SHARING_MODE_CONCURRENT: Result := 'CONCURRENT';
    else
      Result := Format('VkSharingMode(%d)', [Ord(Value)]);
  end;
end;

operator := (Value: VkPresentModeKHR): string; inline;
begin
  case Value of
    VK_PRESENT_MODE_IMMEDIATE_KHR: Result := 'IMMEDIATE';
    VK_PRESENT_MODE_MAILBOX_KHR: Result := 'MAILBOX';
    VK_PRESENT_MODE_FIFO_KHR: Result := 'FIFO';
    VK_PRESENT_MODE_FIFO_RELAXED_KHR: Result := 'FIFO_RELAXED';
    else
      Result := Format('VkPresentModeKHR(%d)', [Ord(Value)]);
  end;
end;

operator := (Value: VkCompositeAlphaFlagBitsKHR): string; inline;
begin
  case Value of
    VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR: Result := 'OPAQUE';
    VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR: Result := 'PRE_MULTIPLIED';
    VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR: Result := 'POST_MULTIPLIED';
    VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR: Result := 'INHERIT';
    else
      Result := Format('VkCompositeAlpha(%d)', [Ord(Value)]);
  end;
end;


procedure TVkApplicationInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_APPLICATION_INFO;
end;

function TVkApplicationInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%spApplicationName: %s' + LineEnding + '%sapplicationVersion: %d.%d.%d' + LineEnding + '%spEngineName: %s' + LineEnding + '%sengineVersion: %d.%d.%d' + LineEnding + '%sapiVersion: %d.%d.%d' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, string(pApplicationName), Indent, VK_VERSION_MAJOR(applicationVersion), VK_VERSION_MINOR(applicationVersion), VK_VERSION_PATCH(applicationVersion), Indent, string(pEngineName), Indent, VK_VERSION_MAJOR(engineVersion), VK_VERSION_MINOR(engineVersion), VK_VERSION_PATCH(engineVersion), Indent, VK_VERSION_MAJOR(apiVersion), VK_VERSION_MINOR(apiVersion), VK_VERSION_PATCH(apiVersion), Indent]);
end;

procedure TVkInstanceCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
end;

function TVkInstanceCreateInfoHelper.ToFormattedString(const Indent: string): string;
var
  layersStr, extensionsStr: string;
  i: integer;
begin
  layersStr := '';
  if ppEnabledLayerNames <> nil then
    for i := 0 to enabledLayerCount - 1 do
      layersStr := layersStr + Indent + '  ' + string(ppEnabledLayerNames[i]) + LineEnding;

  extensionsStr := '';
  if ppEnabledExtensionNames <> nil then
    for i := 0 to enabledExtensionCount - 1 do
      extensionsStr := extensionsStr + Indent + '  ' + string(ppEnabledExtensionNames[i]) + LineEnding;

  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%spApplicationInfo: %p' + LineEnding + '%senabledLayerCount: %d' + LineEnding + '%sppEnabledLayerNames:%s' + LineEnding + '%senabledExtensionCount: %d' + LineEnding + '%sppEnabledExtensionNames:%s' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags, Indent, Pointer(pApplicationInfo), Indent, enabledLayerCount, layersStr, Indent, enabledExtensionCount, extensionsStr, Indent]);
end;

procedure TVkDebugUtilsMessengerCreateInfoEXTHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT;
end;

function TVkDebugUtilsMessengerCreateInfoEXTHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%smessageSeverity: $%08x' + LineEnding + '%smessageType: $%08x' + LineEnding + '%spfnUserCallback: %p' + LineEnding + '%spUserData: %p' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags, Indent, messageSeverity, Indent, messageType, Indent, Pointer(pfnUserCallback), Indent, Pointer(pUserData), Indent]);
end;

procedure TVkPhysicalDeviceFeaturesHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

function TVkPhysicalDeviceFeaturesHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%srobustBufferAccess: %s' + LineEnding + '%sfullDrawIndexUint32: %s' + LineEnding + '%simageCubeArray: %s' + LineEnding + '%sindependentBlend: %s' + LineEnding + '%sgeometryShader: %s' + LineEnding + '%stessellationShader: %s' + LineEnding + '%ssampleRateShading: %s' + LineEnding + '%sdualSrcBlend: %s' + LineEnding + '%slogicOp: %s' + LineEnding + '%smultiDrawIndirect: %s' + LineEnding + '%sdrawIndirectFirstInstance: %s' + LineEnding + '%sdepthClamp: %s' + LineEnding + '%sdepthBiasClamp: %s' + LineEnding + '%sfillModeNonSolid: %s' + LineEnding + '%sdepthBounds: %s' + LineEnding + '%swideLines: %s' + LineEnding + '%slargePoints: %s' + LineEnding + '%salphaToOne: %s' + LineEnding + '%smultiViewport: %s' +
    LineEnding + '%ssamplerAnisotropy: %s' + LineEnding + '%stextureCompressionETC2: %s' + LineEnding + '%stextureCompressionASTC_LDR: %s' + LineEnding + '%stextureCompressionBC: %s' + LineEnding + '%socclusionQueryPrecise: %s' + LineEnding + '%spipelineStatisticsQuery: %s' + LineEnding + '%svertexPipelineStoresAndAtomics: %s' + LineEnding + '%sfragmentStoresAndAtomics: %s' + LineEnding + '%sshaderTessellationAndGeometryPointSize: %s' + LineEnding + '%sshaderImageGatherExtended: %s' + LineEnding + '%sshaderStorageImageExtendedFormats: %s' + LineEnding + '%sshaderStorageImageMultisample: %s' + LineEnding + '%sshaderStorageImageReadWithoutFormat: %s' + LineEnding + '%sshaderStorageImageWriteWithoutFormat: %s' + LineEnding +
    '%sshaderUniformBufferArrayDynamicIndexing: %s' + LineEnding + '%sshaderSampledImageArrayDynamicIndexing: %s' + LineEnding + '%sshaderStorageBufferArrayDynamicIndexing: %s' + LineEnding + '%sshaderStorageImageArrayDynamicIndexing: %s' + LineEnding + '%sshaderClipDistance: %s' + LineEnding + '%sshaderCullDistance: %s' + LineEnding + '%sshaderFloat64: %s' + LineEnding + '%sshaderInt64: %s' + LineEnding + '%sshaderInt16: %s' + LineEnding + '%sshaderResourceResidency: %s' + LineEnding + '%sshaderResourceMinLod: %s' + LineEnding + '%ssparseBinding: %s' + LineEnding + '%ssparseResidencyBuffer: %s' + LineEnding + '%ssparseResidencyImage2D: %s' + LineEnding + '%ssparseResidencyImage3D: %s' + LineEnding + '%ssparseResidency2Samples: %s' + LineEnding +
    '%ssparseResidency4Samples: %s' + LineEnding + '%ssparseResidency8Samples: %s' + LineEnding + '%ssparseResidency16Samples: %s' + LineEnding + '%svariableMultisampleRate: %s' + LineEnding + '%sinheritedQueries: %s' + LineEnding + '%s}', [Indent, Indent, BoolToStr(robustBufferAccess, True), Indent, BoolToStr(fullDrawIndexUint32, True), Indent, BoolToStr(imageCubeArray, True), Indent, BoolToStr(independentBlend, True), Indent, BoolToStr(geometryShader, True), Indent, BoolToStr(tessellationShader, True), Indent, BoolToStr(sampleRateShading, True), Indent, BoolToStr(dualSrcBlend, True), Indent, BoolToStr(logicOp, True), Indent, BoolToStr(multiDrawIndirect, True), Indent, BoolToStr(drawIndirectFirstInstance, True), Indent, BoolToStr(depthClamp, True),
    Indent, BoolToStr(depthBiasClamp, True), Indent, BoolToStr(fillModeNonSolid, True), Indent, BoolToStr(depthBounds, True), Indent, BoolToStr(wideLines, True), Indent, BoolToStr(largePoints, True), Indent, BoolToStr(alphaToOne, True), Indent, BoolToStr(multiViewport, True), Indent, BoolToStr(samplerAnisotropy, True), Indent, BoolToStr(textureCompressionETC2, True), Indent, BoolToStr(textureCompressionASTC_LDR, True), Indent, BoolToStr(textureCompressionBC, True), Indent, BoolToStr(occlusionQueryPrecise, True), Indent, BoolToStr(pipelineStatisticsQuery, True), Indent, BoolToStr(vertexPipelineStoresAndAtomics, True), Indent, BoolToStr(fragmentStoresAndAtomics, True), Indent, BoolToStr(shaderTessellationAndGeometryPointSize, True), Indent,
    BoolToStr(shaderImageGatherExtended, True), Indent, BoolToStr(shaderStorageImageExtendedFormats, True), Indent, BoolToStr(shaderStorageImageMultisample, True), Indent, BoolToStr(shaderStorageImageReadWithoutFormat, True), Indent, BoolToStr(shaderStorageImageWriteWithoutFormat, True), Indent, BoolToStr(shaderUniformBufferArrayDynamicIndexing, True), Indent, BoolToStr(shaderSampledImageArrayDynamicIndexing, True), Indent, BoolToStr(shaderStorageBufferArrayDynamicIndexing, True), Indent, BoolToStr(shaderStorageImageArrayDynamicIndexing, True), Indent, BoolToStr(shaderClipDistance, True), Indent, BoolToStr(shaderCullDistance, True), Indent, BoolToStr(shaderFloat64, True), Indent, BoolToStr(shaderInt64, True), Indent, BoolToStr(shaderInt16, True),
    Indent, BoolToStr(shaderResourceResidency, True), Indent, BoolToStr(shaderResourceMinLod, True), Indent, BoolToStr(sparseBinding, True), Indent, BoolToStr(sparseResidencyBuffer, True), Indent, BoolToStr(sparseResidencyImage2D, True), Indent, BoolToStr(sparseResidencyImage3D, True), Indent, BoolToStr(sparseResidency2Samples, True), Indent, BoolToStr(sparseResidency4Samples, True), Indent, BoolToStr(sparseResidency8Samples, True), Indent, BoolToStr(sparseResidency16Samples, True), Indent, BoolToStr(variableMultisampleRate, True), Indent, BoolToStr(inheritedQueries, True), Indent]);
end;

procedure TVkDeviceQueueCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
end;

function TVkDeviceQueueCreateInfoHelper.ToFormattedString(const Indent: string): string;
var
  prioritiesStr: string;
  i: integer;
begin
  prioritiesStr := '';
  if pQueuePriorities <> nil then
    for i := 0 to queueCount - 1 do
      prioritiesStr := prioritiesStr + Indent + '  ' + FloatToStr(pQueuePriorities[i]) + LineEnding;

  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%squeueFamilyIndex: %d' + LineEnding + '%squeueCount: %d' + LineEnding + '%spQueuePriorities:%s' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags, Indent, queueFamilyIndex, Indent, queueCount, prioritiesStr, Indent]);
end;

procedure TVkDeviceCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
end;

function TVkDeviceCreateInfoHelper.ToFormattedString(const Indent: string): string;
var
  queuesStr, layersStr, extensionsStr: string;
  i: integer;
begin
  queuesStr := '';
  if pQueueCreateInfos <> nil then
    for i := 0 to queueCreateInfoCount - 1 do
      queuesStr := queuesStr + pQueueCreateInfos[i].ToFormattedString(Indent + '  ') + LineEnding;

  layersStr := '';
  if ppEnabledLayerNames <> nil then
    for i := 0 to enabledLayerCount - 1 do
      layersStr := layersStr + Indent + '  ' + string(ppEnabledLayerNames[i]) + LineEnding;

  extensionsStr := '';
  if ppEnabledExtensionNames <> nil then
    for i := 0 to enabledExtensionCount - 1 do
      extensionsStr := extensionsStr + Indent + '  ' + string(ppEnabledExtensionNames[i]) + LineEnding;

  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%squeueCreateInfoCount: %d' + LineEnding + '%spQueueCreateInfos:%s' + LineEnding + '%senabledLayerCount: %d' + LineEnding + '%sppEnabledLayerNames:%s' + LineEnding + '%senabledExtensionCount: %d' + LineEnding + '%sppEnabledExtensionNames:%s' + LineEnding + '%spEnabledFeatures: %p' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags, Indent, queueCreateInfoCount, queuesStr, Indent, enabledLayerCount, layersStr, Indent, enabledExtensionCount, extensionsStr, Indent, Pointer(pEnabledFeatures), Indent]);
end;

procedure TVkSurfaceCapabilitiesKHRHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

function TVkSurfaceCapabilitiesKHRHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%sminImageCount: %d' + LineEnding + '%smaxImageCount: %d' + LineEnding + '%scurrentExtent: %dx%d' + LineEnding + '%sminImageExtent: %dx%d' + LineEnding + '%smaxImageExtent: %dx%d' + LineEnding + '%smaxImageArrayLayers: %d' + LineEnding + '%ssupportedTransforms: $%08x' + LineEnding + '%scurrentTransform: %d' + LineEnding + '%ssupportedCompositeAlpha: $%08x' + LineEnding + '%ssupportedUsageFlags: $%08x' + LineEnding + '%s}', [Indent, Indent, minImageCount, Indent, maxImageCount, Indent, currentExtent.Width, currentExtent.Height, Indent, minImageExtent.Width, minImageExtent.Height, Indent, maxImageExtent.Width, maxImageExtent.Height, Indent, maxImageArrayLayers, Indent, supportedTransforms, Indent, Ord(currentTransform), Indent, supportedCompositeAlpha, Indent, supportedUsageFlags, Indent]);
end;

procedure TVkSurfaceFormatKHRHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

function TVkSurfaceFormatKHRHelper.ToFormattedString(const Indent: string): string;
begin
  Result := SysUtils.Format('%s{' + LineEnding + '%sformat: %d' + LineEnding + '%scolorSpace: %d' + LineEnding + '%s}', [Indent, Indent, Ord(format), Indent, Ord(colorSpace), Indent]);
end;

function TVkPresentModeKHRHelper.ToString: string;
begin
  case Self of
    VK_PRESENT_MODE_IMMEDIATE_KHR: Result := 'VK_PRESENT_MODE_IMMEDIATE_KHR';
    VK_PRESENT_MODE_MAILBOX_KHR: Result := 'VK_PRESENT_MODE_MAILBOX_KHR';
    VK_PRESENT_MODE_FIFO_KHR: Result := 'VK_PRESENT_MODE_FIFO_KHR';
    VK_PRESENT_MODE_FIFO_RELAXED_KHR: Result := 'VK_PRESENT_MODE_FIFO_RELAXED_KHR';
    else
      Result := 'Unknown';
  end;
end;

procedure TVkSwapchainCreateInfoKHRHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
end;

function TVkSwapchainCreateInfoKHRHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%ssurface: %p' + LineEnding + '%sminImageCount: %d' + LineEnding + '%simageFormat: %d' + LineEnding + '%simageColorSpace: %d' + LineEnding + '%simageExtent: %dx%d' + LineEnding + '%simageArrayLayers: %d' + LineEnding + '%simageUsage: $%08x' + LineEnding + '%simageSharingMode: %d' + LineEnding + '%squeueFamilyIndexCount: %d' + LineEnding + '%spQueueFamilyIndices: %p' + LineEnding + '%spreTransform: %d' + LineEnding + '%scompositeAlpha: %d' + LineEnding + '%spresentMode: %d' + LineEnding + '%sclipped: %s' + LineEnding + '%soldSwapchain: %p' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext),
    Indent, flags, Indent, Pointer(surface), Indent, minImageCount, Indent, Ord(imageFormat), Indent, Ord(imageColorSpace), Indent, imageExtent.Width, imageExtent.Height, Indent, imageArrayLayers, Indent, imageUsage, Indent, Ord(imageSharingMode), Indent, queueFamilyIndexCount, Indent, Pointer(pQueueFamilyIndices), Indent, Ord(preTransform), Indent, Ord(compositeAlpha), Indent, Ord(presentMode), Indent, BoolToStr(clipped, True), Indent, Pointer(oldSwapchain), Indent]);
end;

procedure TVkImageViewCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
end;

function TVkImageViewCreateInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := SysUtils.Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%simage: %p' + LineEnding + '%sviewType: %d' + LineEnding + '%sformat: %d' + LineEnding + '%scomponents: (r: %d, g: %d, b: %d, a: %d)' + LineEnding + '%ssubresourceRange: (aspectMask: $%08x, baseMipLevel: %d, levelCount: %d, baseArrayLayer: %d, layerCount: %d)' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags, Indent, Pointer(image), Indent, Ord(viewType), Indent, Ord(format), Indent, Ord(Components.r), Ord(Components.g), Ord(Components.b), Ord(Components.a), Indent, subresourceRange.aspectMask, subresourceRange.baseMipLevel, subresourceRange.levelCount, subresourceRange.baseArrayLayer, subresourceRange.layerCount, Indent]);
end;

procedure TVkAttachmentDescriptionHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

function TVkAttachmentDescriptionHelper.ToFormattedString(const Indent: string): string;
begin
  Result := SysUtils.Format('%s{' + LineEnding + '%sflags: $%08x' + LineEnding + '%sformat: %d' + LineEnding + '%ssamples: %d' + LineEnding + '%sloadOp: %d' + LineEnding + '%sstoreOp: %d' + LineEnding + '%sstencilLoadOp: %d' + LineEnding + '%sstencilStoreOp: %d' + LineEnding + '%sinitialLayout: %d' + LineEnding + '%sfinalLayout: %d' + LineEnding + '%s}', [Indent, Indent, flags, Indent, Ord(format), Indent, Ord(samples), Indent, Ord(loadOp), Indent, Ord(storeOp), Indent, Ord(stencilLoadOp), Indent, Ord(stencilStoreOp), Indent, Ord(initialLayout), Indent, Ord(finalLayout), Indent]);
end;

procedure TVkAttachmentReferenceHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

function TVkAttachmentReferenceHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%sattachment: %d' + LineEnding + '%slayout: %d' + LineEnding + '%s}', [Indent, Indent, attachment, Indent, Ord(layout), Indent]);
end;

procedure TVkSubpassDescriptionHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

function TVkSubpassDescriptionHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%sflags: $%08x' + LineEnding + '%spipelineBindPoint: %d' + LineEnding + '%sinputAttachmentCount: %d' + LineEnding + '%spInputAttachments: %p' + LineEnding + '%scolorAttachmentCount: %d' + LineEnding + '%spColorAttachments: %p' + LineEnding + '%spResolveAttachments: %p' + LineEnding + '%spDepthStencilAttachment: %p' + LineEnding + '%spreserveAttachmentCount: %d' + LineEnding + '%spPreserveAttachments: %p' + LineEnding + '%s}', [Indent, Indent, flags, Indent, Ord(pipelineBindPoint), Indent, inputAttachmentCount, Indent, Pointer(pInputAttachments), Indent, colorAttachmentCount, Indent, Pointer(pColorAttachments), Indent, Pointer(pResolveAttachments), Indent, Pointer(pDepthStencilAttachment), Indent, preserveAttachmentCount, Indent, Pointer(pPreserveAttachments), Indent]);
end;

procedure TVkRenderPassCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
end;

function TVkRenderPassCreateInfoHelper.ToFormattedString(const Indent: string): string;
var
  attachmentsStr, subpassesStr, dependenciesStr: string;
  i: integer;
  subpassDependencyCursor: PVkSubpassDependency;
begin
  attachmentsStr := '';
  if pAttachments <> nil then
    for i := 0 to attachmentCount - 1 do
      attachmentsStr := attachmentsStr + pAttachments[i].ToFormattedString(Indent + '  ') + LineEnding;

  subpassesStr := '';
  if pSubpasses <> nil then
    for i := 0 to subpassCount - 1 do
      subpassesStr := subpassesStr + pSubpasses[i].ToFormattedString(Indent + '  ') + LineEnding;

  dependenciesStr := '';
  subpassDependencyCursor := pDependencies;
  if pDependencies <> nil then

    for i := 0 to dependencyCount - 1 do
    begin
      dependenciesStr := dependenciesStr + VkSubpassDependency(subpassDependencyCursor^).ToFormattedString(Indent + '  ') + LineEnding;
      Inc(subpassDependencyCursor);
    end;

  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%sattachmentCount: %d' + LineEnding + '%spAttachments:%s' + LineEnding + '%ssubpassCount: %d' + LineEnding + '%spSubpasses:%s' + LineEnding + '%sdependencyCount: %d' + LineEnding + '%spDependencies:%s' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags, Indent, attachmentCount, attachmentsStr, Indent, subpassCount, subpassesStr, Indent, dependencyCount, dependenciesStr, Indent]);
end;

procedure TVkFramebufferCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
end;

function TVkFramebufferCreateInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%srenderPass: %p' + LineEnding + '%sattachmentCount: %d' + LineEnding + '%spAttachments: %p' + LineEnding + '%swidth: %d' + LineEnding + '%sheight: %d' + LineEnding + '%slayers: %d' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags, Indent, Pointer(renderPass), Indent, attachmentCount, Indent, Pointer(pAttachments), Indent, Width, Indent, Height, Indent, layers, Indent]);
end;

procedure TVkShaderModuleCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
end;

function TVkShaderModuleCreateInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%scodeSize: %d' + LineEnding + '%spCode: %p' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags, Indent, codeSize, Indent, Pointer(pCode), Indent]);
end;

procedure TVkPipelineShaderStageCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
end;

function TVkPipelineShaderStageCreateInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding +//*
    '%ssType: %s' + LineEnding +//*
    '%spNext: %p' + LineEnding +//*
    '%sflags: $%08x' + LineEnding +//*
    '%sstage: $%08x' + LineEnding +//*
    '%smodule: %p' + LineEnding +//*
    '%spName: %s' + LineEnding +//*
    '%spSpecializationInfo: %p' +//*
    LineEnding + '%s}', [//*
    Indent, Indent, string(sType),//*
    Indent, Pointer(pNext),//*
    Indent, flags,//*
    Indent, Ord(stage),//*
    Indent, Pointer(module),//*
    Indent, string(pName),//*
    Indent, Pointer(pSpecializationInfo),//*
    Indent]);
end;

procedure TVkPipelineVertexInputStateCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
end;

function TVkPipelineVertexInputStateCreateInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%svertexBindingDescriptionCount: %d' + LineEnding + '%spVertexBindingDescriptions: %p' + LineEnding + '%svertexAttributeDescriptionCount: %d' + LineEnding + '%spVertexAttributeDescriptions: %p' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags, Indent, vertexBindingDescriptionCount, Indent, Pointer(pVertexBindingDescriptions), Indent, vertexAttributeDescriptionCount, Indent, Pointer(pVertexAttributeDescriptions), Indent]);
end;

procedure TVkPipelineInputAssemblyStateCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
end;

function TVkPipelineInputAssemblyStateCreateInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%stopology: %d' + LineEnding + '%sprimitiveRestartEnable: %s' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags, Indent, Ord(topology), Indent, BoolToStr(primitiveRestartEnable, True), Indent]);
end;

procedure TVkPipelineViewportStateCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
end;

function TVkPipelineViewportStateCreateInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%sviewportCount: %d' + LineEnding + '%spViewports: %p' + LineEnding + '%sscissorCount: %d' + LineEnding + '%spScissors: %p' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags, Indent, viewportCount, Indent, Pointer(pViewports), Indent, scissorCount, Indent, Pointer(pScissors), Indent]);
end;

procedure TVkPipelineRasterizationStateCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
end;

function TVkPipelineRasterizationStateCreateInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%sdepthClampEnable: %s' + LineEnding + '%srasterizerDiscardEnable: %s' + LineEnding + '%spolygonMode: %d' + LineEnding + '%scullMode: $%08x' + LineEnding + '%sfrontFace: %d' + LineEnding + '%sdepthBiasEnable: %s' + LineEnding + '%sdepthBiasConstantFactor: %.3f' + LineEnding + '%sdepthBiasClamp: %.3f' + LineEnding + '%sdepthBiasSlopeFactor: %.3f' + LineEnding + '%slineWidth: %.3f' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags, Indent, BoolToStr(depthClampEnable, True), Indent, BoolToStr(rasterizerDiscardEnable, True), Indent, Ord(polygonMode), Indent, cullMode, Indent, Ord(frontFace), Indent, BoolToStr(depthBiasEnable, True), Indent, depthBiasConstantFactor, Indent, depthBiasClamp, Indent, depthBiasSlopeFactor, Indent, lineWidth, Indent]);
end;

procedure TVkPipelineMultisampleStateCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
end;

function TVkPipelineMultisampleStateCreateInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%srasterizationSamples: %d' + LineEnding + '%ssampleShadingEnable: %s' + LineEnding + '%sminSampleShading: %.3f' + LineEnding + '%spSampleMask: %p' + LineEnding + '%salphaToCoverageEnable: %s' + LineEnding + '%salphaToOneEnable: %s' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags, Indent, Ord(rasterizationSamples), Indent, BoolToStr(sampleShadingEnable, True), Indent, minSampleShading, Indent, Pointer(pSampleMask), Indent, BoolToStr(alphaToCoverageEnable, True), Indent, BoolToStr(alphaToOneEnable, True), Indent]);
end;

procedure TVkPipelineColorBlendAttachmentStateHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

function TVkPipelineColorBlendAttachmentStateHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%sblendEnable: %s' + LineEnding + '%ssrcColorBlendFactor: %d' + LineEnding + '%sdstColorBlendFactor: %d' + LineEnding + '%scolorBlendOp: %d' + LineEnding + '%ssrcAlphaBlendFactor: %d' + LineEnding + '%sdstAlphaBlendFactor: %d' + LineEnding + '%salphaBlendOp: %d' + LineEnding + '%scolorWriteMask: $%08x' + LineEnding + '%s}', [Indent, Indent, BoolToStr(blendEnable, True), Indent, Ord(srcColorBlendFactor), Indent, Ord(dstColorBlendFactor), Indent, Ord(colorBlendOp), Indent, Ord(srcAlphaBlendFactor), Indent, Ord(dstAlphaBlendFactor), Indent, Ord(alphaBlendOp), Indent, colorWriteMask, Indent]);
end;

procedure TVkPipelineColorBlendStateCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO;
end;

function TVkPipelineColorBlendStateCreateInfoHelper.ToFormattedString(const Indent: string): string;
var
  attachmentsStr: string;
  i: integer;
begin
  attachmentsStr := '';
  if pAttachments <> nil then
    for i := 0 to attachmentCount - 1 do
      attachmentsStr := attachmentsStr + pAttachments[i].ToFormattedString(Indent + '  ') + LineEnding;

  Result := Format('%s{' + LineEnding + //*
    '%ssType: %s' + LineEnding +//*
    '%spNext: %p' + LineEnding +   //*
    '%sflags: $%08x' + LineEnding +   //*
    '%slogicOpEnable: %s' + LineEnding + //*
    '%slogicOp: %d' + LineEnding +//*
    '%sattachmentCount: %d' + LineEnding +//*
    '%spAttachments:%s' + LineEnding +//*
    '%sblendConstants: [%.3f, %.3f, %.3f, %.3f]' + LineEnding +//*
    '%s}', [//*
    Indent,//*
    Indent, string(sType), Indent, Pointer(pNext),//*
    Indent, flags,//*
    Indent, BoolToStr(logicOpEnable, True),//*
    Indent, Ord(logicOp),//*
    Indent, attachmentCount,//*
    attachmentsStr,//*
    Indent, //*
    blendConstants[0], blendConstants[1], blendConstants[2], blendConstants[3],//*
    Indent]);
end;

procedure TVkPipelineDynamicStateCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO;
end;

function TVkPipelineDynamicStateCreateInfoHelper.ToFormattedString(const Indent: string): string;
var
  statesStr: string;
  i: integer;
begin
  statesStr := '';
  if pDynamicStates <> nil then
    for i := 0 to dynamicStateCount - 1 do
      statesStr := statesStr + Indent + '  ' + string(pDynamicStates[i]) + LineEnding;

  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%sdynamicStateCount: %d' + LineEnding + '%spDynamicStates:%s' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags, Indent, dynamicStateCount, statesStr, Indent]);
end;

procedure TVkPipelineLayoutCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
end;

function TVkPipelineLayoutCreateInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%ssetLayoutCount: %d' + LineEnding + '%spSetLayouts: %p' + LineEnding + '%spushConstantRangeCount: %d' + LineEnding + '%spPushConstantRanges: %p' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags, Indent, setLayoutCount, Indent, Pointer(pSetLayouts), Indent, pushConstantRangeCount, Indent, Pointer(pPushConstantRanges), Indent]);
end;

procedure TVkGraphicsPipelineCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO;
end;

function TVkGraphicsPipelineCreateInfoHelper.ToFormattedString(const Indent: string): string;
var
  stagesStr: string;
  i: integer;
begin
  stagesStr := '';
  if pStages <> nil then
    for i := 0 to stageCount - 1 do
      stagesStr := stagesStr + pStages[i].ToFormattedString(Indent + '  ') + LineEnding;

  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%sstageCount: %d' + LineEnding + '%spStages:%s' + LineEnding + '%spVertexInputState: %p' + LineEnding + '%spInputAssemblyState: %p' + LineEnding + '%spTessellationState: %p' + LineEnding + '%spViewportState: %p' + LineEnding + '%spRasterizationState: %p' + LineEnding + '%spMultisampleState: %p' + LineEnding + '%spDepthStencilState: %p' + LineEnding + '%spColorBlendState: %p' + LineEnding + '%spDynamicState: %p' + LineEnding + '%slayout: %p' + LineEnding + '%srenderPass: %p' + LineEnding + '%ssubpass: %d' + LineEnding + '%sbasePipelineHandle: %p' + LineEnding + '%sbasePipelineIndex: %d' + LineEnding + '%s}',
    [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags, Indent, stageCount, stagesStr, Indent, Pointer(pVertexInputState), Indent, Pointer(pInputAssemblyState), Indent, Pointer(pTessellationState), Indent, Pointer(pViewportState), Indent, Pointer(pRasterizationState), Indent, Pointer(pMultisampleState), Indent, Pointer(pDepthStencilState), Indent, Pointer(pColorBlendState), Indent, Pointer(pDynamicState), Indent, Pointer(layout), Indent, Pointer(renderPass), Indent, subpass, Indent, Pointer(basePipelineHandle), Indent, basePipelineIndex, Indent]);
end;

procedure TVkCommandPoolCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
end;

function TVkCommandPoolCreateInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%squeueFamilyIndex: %d' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags, Indent, queueFamilyIndex, Indent]);
end;

procedure TVkCommandBufferAllocateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
end;

function TVkCommandBufferAllocateInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%scommandPool: %p' + LineEnding + '%slevel: %d' + LineEnding + '%scommandBufferCount: %d' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, Pointer(commandPool), Indent, Ord(level), Indent, commandBufferCount, Indent]);
end;

procedure TVkCommandBufferBeginInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
end;

function TVkCommandBufferBeginInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%spInheritanceInfo: %p' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags, Indent, Pointer(pInheritanceInfo), Indent]);
end;

procedure TVkRenderPassBeginInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
end;

function TVkRenderPassBeginInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%srenderPass: %p' + LineEnding + '%sframebuffer: %p' + LineEnding + '%srenderArea: offset(%d,%d) extent(%d,%d)' + LineEnding + '%sclearValueCount: %d' + LineEnding + '%spClearValues: %p' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, Pointer(renderPass), Indent, Pointer(framebuffer), Indent, renderArea.offset.x, renderArea.offset.y, renderArea.extent.Width, renderArea.extent.Height, Indent, clearValueCount, Indent, Pointer(pClearValues), Indent]);
end;

procedure TVkSemaphoreCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
end;

function TVkSemaphoreCreateInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags, Indent]);
end;

procedure TVkFenceCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
end;

function TVkFenceCreateInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags, Indent]);
end;

procedure TVkSubmitInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_SUBMIT_INFO;
end;

function TVkSubmitInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%swaitSemaphoreCount: %d' + LineEnding + '%spWaitSemaphores: %p' + LineEnding + '%spWaitDstStageMask: %p' + LineEnding + '%scommandBufferCount: %d' + LineEnding + '%spCommandBuffers: %p' + LineEnding + '%ssignalSemaphoreCount: %d' + LineEnding + '%spSignalSemaphores: %p' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, waitSemaphoreCount, Indent, Pointer(pWaitSemaphores), Indent, Pointer(pWaitDstStageMask), Indent, commandBufferCount, Indent, Pointer(pCommandBuffers), Indent, signalSemaphoreCount, Indent, Pointer(pSignalSemaphores), Indent]);
end;

procedure TVkPresentInfoKHRHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
end;

function TVkPresentInfoKHRHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%swaitSemaphoreCount: %d' + LineEnding + '%spWaitSemaphores: %p' + LineEnding + '%sswapchainCount: %d' + LineEnding + '%spSwapchains: %p' + LineEnding + '%spImageIndices: %p' + LineEnding + '%spResults: %p' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, waitSemaphoreCount, Indent, Pointer(pWaitSemaphores), Indent, swapchainCount, Indent, Pointer(pSwapchains), Indent, Pointer(pImageIndices), Indent, Pointer(pResults), Indent]);
end;

procedure TVkVertexInputBindingDescriptionHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

function TVkVertexInputBindingDescriptionHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%sbinding: %d' + LineEnding + '%sstride: %d' + LineEnding + '%sinputRate: %d' + LineEnding + '%s}', [Indent, Indent, binding, Indent, stride, Indent, Ord(inputRate), Indent]);
end;

procedure TVkVertexInputAttributeDescriptionHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

function TVkVertexInputAttributeDescriptionHelper.ToFormattedString(const Indent: string): string;
begin
  Result := SysUtils.Format('%s{' + LineEnding + '%slocation: %d' + LineEnding + '%sbinding: %d' + LineEnding + '%sformat: %d' + LineEnding + '%soffset: %d' + LineEnding + '%s}', [Indent, Indent, location, Indent, binding, Indent, Ord(format), Indent, offset, Indent]);
end;

procedure TVkBufferCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
end;

function TVkBufferCreateInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%ssize: %d' + LineEnding + '%susage: $%08x' + LineEnding + '%ssharingMode: %d' + LineEnding + '%squeueFamilyIndexCount: %d' + LineEnding + '%spQueueFamilyIndices: %p' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags, Indent, size, Indent, usage, Indent, Ord(sharingMode), Indent, queueFamilyIndexCount, Indent, Pointer(pQueueFamilyIndices), Indent]);
end;

procedure TVkMemoryAllocateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
end;

function TVkMemoryAllocateInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sallocationSize: %d' + LineEnding + '%smemoryTypeIndex: %d' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, allocationSize, Indent, memoryTypeIndex, Indent]);
end;

procedure TVkDescriptorSetLayoutBindingHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

function TVkDescriptorSetLayoutBindingHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%sbinding: %d' + LineEnding + '%sdescriptorType: %d' + LineEnding + '%sdescriptorCount: %d' + LineEnding + '%sstageFlags: $%08x' + LineEnding + '%spImmutableSamplers: %p' + LineEnding + '%s}', [Indent, Indent, binding, Indent, Ord(descriptorType), Indent, descriptorCount, Indent, stageFlags, Indent, Pointer(pImmutableSamplers), Indent]);
end;

procedure TVkDescriptorSetLayoutCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
end;

function TVkDescriptorSetLayoutCreateInfoHelper.ToFormattedString(const Indent: string): string;
var
  bindingsStr: string;
  i: integer;
begin
  bindingsStr := '';
  if pBindings <> nil then
    for i := 0 to bindingCount - 1 do
      bindingsStr := bindingsStr + pBindings[i].ToFormattedString(Indent + '  ') + LineEnding;

  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%sbindingCount: %d' + LineEnding + '%spBindings:%s' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags, Indent, bindingCount, bindingsStr, Indent]);
end;

procedure TVkDescriptorPoolSizeHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

function TVkDescriptorPoolSizeHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%stype: %d' + LineEnding + '%sdescriptorCount: %d' + LineEnding + '%s}', [Indent, Indent, Ord(descriptorType), Indent, descriptorCount, Indent]);
end;

procedure TVkDescriptorPoolCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
end;

function TVkDescriptorPoolCreateInfoHelper.ToFormattedString(const Indent: string): string;
var
  poolSizesStr: string;
  i: integer;
begin
  poolSizesStr := '';
  if pPoolSizes <> nil then
    for i := 0 to poolSizeCount - 1 do
      poolSizesStr := poolSizesStr + pPoolSizes[i].ToFormattedString(Indent + '  ') + LineEnding;

  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%smaxSets: %d' + LineEnding + '%spoolSizeCount: %d' + LineEnding + '%spPoolSizes:%s' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags, Indent, maxSets, Indent, poolSizeCount, poolSizesStr, Indent]);
end;

procedure TVkDescriptorSetAllocateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
end;

function TVkDescriptorSetAllocateInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sdescriptorPool: %p' + LineEnding + '%sdescriptorSetCount: %d' + LineEnding + '%spSetLayouts: %p' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, Pointer(descriptorPool), Indent, descriptorSetCount, Indent, Pointer(pSetLayouts), Indent]);
end;

procedure TVkWriteDescriptorSetHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
end;

function TVkWriteDescriptorSetHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sdstSet: %p' + LineEnding + '%sdstBinding: %d' + LineEnding + '%sdstArrayElement: %d' + LineEnding + '%sdescriptorCount: %d' + LineEnding + '%sdescriptorType: %d' + LineEnding + '%spImageInfo: %p' + LineEnding + '%spBufferInfo: %p' + LineEnding + '%spTexelBufferView: %p' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, Pointer(dstSet), Indent, dstBinding, Indent, dstArrayElement, Indent, descriptorCount, Indent, Ord(descriptorType), Indent, Pointer(pImageInfo), Indent, Pointer(pBufferInfo), Indent, Pointer(pTexelBufferView), Indent]);
end;

procedure TVkDescriptorBufferInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

function TVkDescriptorBufferInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%sbuffer: %p' + LineEnding + '%soffset: %d' + LineEnding + '%srange: %d' + LineEnding + '%s}', [Indent, Indent, Pointer(buffer), Indent, offset, Indent, range, Indent]);
end;

procedure TVkImageCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
end;

function TVkImageCreateInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := SysUtils.Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%simageType: %d' + LineEnding + '%sformat: %d' + LineEnding + '%sextent: %dx%dx%d' + LineEnding + '%smipLevels: %d' + LineEnding + '%sarrayLayers: %d' + LineEnding + '%ssamples: %d' + LineEnding + '%stiling: %d' + LineEnding + '%susage: $%08x' + LineEnding + '%ssharingMode: %d' + LineEnding + '%squeueFamilyIndexCount: %d' + LineEnding + '%spQueueFamilyIndices: %p' + LineEnding + '%sinitialLayout: %d' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags, Indent, Ord(imageType), Indent, Ord(format), Indent, extent.Width, extent.Height, extent.depth, Indent, mipLevels, Indent, arrayLayers, Indent, Ord(samples), Indent, Ord(tiling), Indent, usage, Indent, Ord(sharingMode), Indent, queueFamilyIndexCount, Indent, Pointer(pQueueFamilyIndices), Indent, Ord(initialLayout), Indent]);
end;

procedure TVkImageMemoryBarrierHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
end;

function TVkImageMemoryBarrierHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%ssrcAccessMask: $%08x' + LineEnding + '%sdstAccessMask: $%08x' + LineEnding + '%soldLayout: %d' + LineEnding + '%snewLayout: %d' + LineEnding + '%ssrcQueueFamilyIndex: %d' + LineEnding + '%sdstQueueFamilyIndex: %d' + LineEnding + '%simage: %p' + LineEnding + '%ssubresourceRange: (aspectMask: $%08x, baseMipLevel: %d, levelCount: %d, baseArrayLayer: %d, layerCount: %d)' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, srcAccessMask, Indent, dstAccessMask, Indent, Ord(oldLayout), Indent, Ord(newLayout), Indent, srcQueueFamilyIndex, Indent, dstQueueFamilyIndex, Indent, Pointer(image), Indent, subresourceRange.aspectMask, subresourceRange.baseMipLevel, subresourceRange.levelCount, subresourceRange.baseArrayLayer, subresourceRange.layerCount, Indent]);
end;

procedure TVkBufferCopyHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

function TVkBufferCopyHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssrcOffset: %d' + LineEnding + '%sdstOffset: %d' + LineEnding + '%ssize: %d' + LineEnding + '%s}', [Indent, Indent, srcOffset, Indent, dstOffset, Indent, size, Indent]);
end;

procedure TVkBufferImageCopyHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

function TVkBufferImageCopyHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%sbufferOffset: %d' + LineEnding + '%sbufferRowLength: %d' + LineEnding + '%sbufferImageHeight: %d' + LineEnding + '%simageSubresource: (aspectMask: $%08x, mipLevel: %d, baseArrayLayer: %d, layerCount: %d)' + LineEnding + '%simageOffset: (%d, %d, %d)' + LineEnding + '%simageExtent: (%d, %d, %d)' + LineEnding + '%s}', [Indent, Indent, bufferOffset, Indent, bufferRowLength, Indent, bufferImageHeight, Indent, imageSubresource.aspectMask, imageSubresource.mipLevel, imageSubresource.baseArrayLayer, imageSubresource.layerCount, Indent, imageOffset.x, imageOffset.y, imageOffset.z, Indent, imageExtent.Width, imageExtent.Height, imageExtent.depth, Indent]);
end;

procedure TVkSamplerCreateInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.sType := VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO;
end;

function TVkSamplerCreateInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssType: %s' + LineEnding + '%spNext: %p' + LineEnding + '%sflags: $%08x' + LineEnding + '%smagFilter: %d' + LineEnding + '%sminFilter: %d' + LineEnding + '%smipmapMode: %d' + LineEnding + '%saddressModeU: %d' + LineEnding + '%saddressModeV: %d' + LineEnding + '%saddressModeW: %d' + LineEnding + '%smipLodBias: %.3f' + LineEnding + '%sanisotropyEnable: %s' + LineEnding + '%smaxAnisotropy: %.3f' + LineEnding + '%scompareEnable: %s' + LineEnding + '%scompareOp: %d' + LineEnding + '%sminLod: %.3f' + LineEnding + '%smaxLod: %.3f' + LineEnding + '%sborderColor: %d' + LineEnding + '%sunnormalizedCoordinates: %s' + LineEnding + '%s}', [Indent, Indent, string(sType), Indent, Pointer(pNext), Indent, flags,
    Indent, Ord(magFilter), Indent, Ord(minFilter), Indent, Ord(mipmapMode), Indent, Ord(addressModeU), Indent, Ord(addressModeV), Indent, Ord(addressModeW), Indent, mipLodBias, Indent, BoolToStr(anisotropyEnable, True), Indent, maxAnisotropy, Indent, BoolToStr(compareEnable, True), Indent, Ord(compareOp), Indent, minLod, Indent, maxLod, Indent, Ord(borderColor), Indent, BoolToStr(unnormalizedCoordinates, True), Indent]);
end;

procedure TVkDescriptorImageInfoHelper.zero;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

function TVkDescriptorImageInfoHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + '%ssampler: %p' + LineEnding + '%simageView: %p' + LineEnding + '%simageLayout: %d' + LineEnding + '%s}', [Indent, Indent, Pointer(sampler), Indent, Pointer(imageView), Indent, Ord(imageLayout), Indent]);
end;

{ ToString implementations for all helpers }
function TVkApplicationInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkInstanceCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkDebugUtilsMessengerCreateInfoEXTHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkPhysicalDeviceFeaturesHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkDeviceQueueCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkDeviceCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkSurfaceCapabilitiesKHRHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkSurfaceFormatKHRHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkSwapchainCreateInfoKHRHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkImageViewCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkAttachmentDescriptionHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkAttachmentReferenceHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkSubpassDescriptionHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkRenderPassCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkFramebufferCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkShaderModuleCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkPipelineShaderStageCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkPipelineVertexInputStateCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkPipelineInputAssemblyStateCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkPipelineViewportStateCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkPipelineRasterizationStateCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkPipelineMultisampleStateCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkPipelineColorBlendAttachmentStateHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkPipelineColorBlendStateCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkPipelineDynamicStateCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkPipelineLayoutCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkGraphicsPipelineCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkCommandPoolCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkCommandBufferAllocateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkCommandBufferBeginInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkRenderPassBeginInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkSemaphoreCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkFenceCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkSubmitInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkPresentInfoKHRHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkVertexInputBindingDescriptionHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkVertexInputAttributeDescriptionHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkBufferCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkMemoryAllocateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkDescriptorSetLayoutBindingHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkDescriptorSetLayoutCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkDescriptorPoolSizeHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkDescriptorPoolCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkDescriptorSetAllocateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkWriteDescriptorSetHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkDescriptorBufferInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkImageCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkImageMemoryBarrierHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkBufferCopyHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkBufferImageCopyHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkSamplerCreateInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkDescriptorImageInfoHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

{ TVkSubpassDependencyHelper }


procedure TVkSubpassDependencyHelper.Zero;
begin
  FillChar(Self, SizeOf(Self), 0);
  // valori di default sensati (non strettamente necessari, ma utili)
  srcSubpass := VK_SUBPASS_EXTERNAL;
  dstSubpass := VK_SUBPASS_EXTERNAL;
  srcStageMask := VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
  dstStageMask := VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT;
  srcAccessMask := 0;
  dstAccessMask := 0;
  dependencyFlags := 0;
end;

function TVkSubpassDependencyHelper.ToString: string;
begin
  Result := ToFormattedString('');
end;

function TVkSubpassDependencyHelper.ToFormattedString(const Indent: string): string;
begin
  Result := Format('%s{' + LineEnding + //*
    '%ssrcSubpass:      %s' + LineEnding +//*
    '%sdstSubpass:      %s' + LineEnding +//*
    '%ssrcStageMask:    %s' + LineEnding +//*
    '%sdstStageMask:    %s' + LineEnding +//*
    '%ssrcAccessMask:   $%08x' + LineEnding +//*
    '%sdstAccessMask:   $%08x' + LineEnding +//*
    '%sdependencyFlags: $%08x' + LineEnding +//*
    '%s}',//*
    [//*
    Indent,//*
    Indent, IfThen(srcSubpass = VK_SUBPASS_EXTERNAL, 'EXTERNAL', IntToStr(srcSubpass)),//*
    Indent, IfThen(dstSubpass = VK_SUBPASS_EXTERNAL, 'EXTERNAL', IntToStr(dstSubpass)),//*
    Indent, string(srcStageMask),//*
    Indent, string(dstStageMask),//*
    Indent, srcAccessMask,//*
    Indent, dstAccessMask,//*
    Indent, dependencyFlags,//*
    Indent  //*
    ]);
end;

end.
