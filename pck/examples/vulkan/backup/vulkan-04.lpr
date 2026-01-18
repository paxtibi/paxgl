program vulkan04;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

uses
  SysUtils,
  typinfo,
  pax.Vulkan,
  pax.glfw;

const
  APP_NAME = '04 - Logical Device';
  ENGINE_NAME = 'No Engine';

var
  instance: VkInstance = VK_NULL_HANDLE;
  physicalDevice: VkPhysicalDevice = VK_NULL_HANDLE;
  device: VkDevice = VK_NULL_HANDLE;

  queueFamilyIndex: uint32 = High(uint32);
  graphicsFamilyFound: boolean = False;

  procedure CheckVulkan(res: VkResult; const msg: string = '');
  begin
    if res <> VK_SUCCESS then
      raise Exception.Create('Vulkan error ' + IntToStr(Ord(res)) + ' (' + msg + ')' + ' - ' + GetEnumName(TypeInfo(VkResult), Ord(res)));
  end;

  // ---------------------------------------------------------------------------
  function FindQueueFamilies(physDev: VkPhysicalDevice): boolean;
  var
    queueFamilyCount: uint32 = 0;
    queueFamilies: array of VkQueueFamilyProperties;
    i: uint32;
  begin
    with getVulkan do
    begin
      vkGetPhysicalDeviceQueueFamilyProperties(physDev, @queueFamilyCount, nil);
      if queueFamilyCount = 0 then exit(False);

      SetLength(queueFamilies, queueFamilyCount);
      vkGetPhysicalDeviceQueueFamilyProperties(physDev, @queueFamilyCount, @queueFamilies[0]);

      for i := 0 to queueFamilyCount - 1 do
      begin
        if (queueFamilies[i].queueFlags and VK_QUEUE_GRAPHICS_BIT) <> 0 then
        begin
          queueFamilyIndex := i;
          graphicsFamilyFound := True;
          break;
        end;
      end;

      Result := graphicsFamilyFound;

    end;
  end;

  // ---------------------------------------------------------------------------
  function IsDeviceSuitable(physDev: VkPhysicalDevice): boolean;
  var
    deviceProperties: VkPhysicalDeviceProperties;
  begin
    with getVulkan do
    begin
      vkGetPhysicalDeviceProperties(physDev, @deviceProperties);

      WriteLn('Device: ', deviceProperties.deviceName,
        ' (type: ', Ord(deviceProperties.deviceType), ')');

      Result := FindQueueFamilies(physDev);
    end;
  end;

  // ---------------------------------------------------------------------------
  procedure PickPhysicalDevice;
  var
    deviceCount: uint32 = 0;
    devices: array of VkPhysicalDevice;
    i: uint32;
  begin
    with getVulkan do
    begin
      CheckVulkan(vkEnumeratePhysicalDevices(instance, @deviceCount, nil), 'vkEnumeratePhysicalDevices');
      if deviceCount = 0 then
        raise Exception.Create('failed to find GPUs with Vulkan support!');

      SetLength(devices, deviceCount);
      CheckVulkan(vkEnumeratePhysicalDevices(instance, @deviceCount, @devices[0]), 'vkEnumeratePhysicalDevices');

      for i := 0 to deviceCount - 1 do
      begin
        if IsDeviceSuitable(devices[i]) then
        begin
          physicalDevice := devices[i];
          break;
        end;
      end;

      if physicalDevice = VK_NULL_HANDLE then
        raise Exception.Create('failed to find a suitable GPU!');
    end;
  end;

  // ---------------------------------------------------------------------------
  procedure CreateLogicalDevice;
  var
    queueCreateInfo: VkDeviceQueueCreateInfo;
    deviceFeatures: VkPhysicalDeviceFeatures;
    deviceCreateInfo: VkDeviceCreateInfo;
    queuePriorities: single = 1.0;
    enabledLayers: array of pansichar;
    enabledExtensions: array of pansichar;
  begin
    with getVulkan do
    begin
      FillChar(queueCreateInfo, SizeOf(queueCreateInfo), 0);
      queueCreateInfo.sType := VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
      queueCreateInfo.queueFamilyIndex := queueFamilyIndex;
      queueCreateInfo.queueCount := 1;
      queueCreateInfo.pQueuePriorities := @queuePriorities;

      FillChar(deviceFeatures, SizeOf(deviceFeatures), 0);

      FillChar(deviceCreateInfo, SizeOf(deviceCreateInfo), 0);
      deviceCreateInfo.sType := VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
      deviceCreateInfo.pQueueCreateInfos := @queueCreateInfo;
      deviceCreateInfo.queueCreateInfoCount := 1;
      deviceCreateInfo.pEnabledFeatures := @deviceFeatures;
      deviceCreateInfo.enabledLayerCount := 0;
      deviceCreateInfo.ppEnabledLayerNames := nil;

      deviceCreateInfo.enabledExtensionCount := 0;
      deviceCreateInfo.ppEnabledExtensionNames := nil;

      CheckVulkan(vkCreateDevice(physicalDevice, @deviceCreateInfo, nil, device), 'vkCreateDevice');
      WriteLn('Logical device creato con successo!');
    end;
  end;


  procedure CreateVulkanInstance(out inst: VkInstance);
  const
    ValidationLayers: array[0..0] of pansichar = ('VK_LAYER_KHRONOS_validation');
  var
    appInfo: VkApplicationInfo;
    createInfo: VkInstanceCreateInfo;
    glfwExtensions: PPAnsiChar;
    glfwExtensionCount: uint32;
    extensions: array of pansichar;
    i: integer;
    res: VkResult;
    enableValidationLayers: boolean = True;
  begin
    with getGLFW, getVulkan do
    begin
      // 1. Informazioni applicazione
      FillChar(appInfo, SizeOf(appInfo), 0);
      appInfo.sType := VK_STRUCTURE_TYPE_APPLICATION_INFO;
      appInfo.pApplicationName := pansichar(APP_NAME);
      appInfo.applicationVersion := VK_MAKE_VERSION(1, 0, 0);
      appInfo.pEngineName := pansichar(ENGINE_NAME);
      appInfo.engineVersion := VK_MAKE_VERSION(1, 0, 0);
      appInfo.apiVersion := VK_API_VERSION_1_0;

      // 2. Ottieni estensioni richieste da GLFW
      glfwExtensions := glfwGetRequiredInstanceExtensions(glfwExtensionCount);

      // 3. Prepara array estensioni
      SetLength(extensions, glfwExtensionCount);
      for i := 0 to glfwExtensionCount - 1 do
        extensions[i] := glfwExtensions[i];

      // Opzionale: aggiungi estensioni extra se necessario (es. debug utils)
      // extensions[glfwExtensionCount] := VK_EXT_DEBUG_UTILS_EXTENSION_NAME;
      // Inc(glfwExtensionCount);

      // 4. Struttura creazione instance
      FillChar(createInfo, SizeOf(createInfo), 0);
      createInfo.sType := VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
      createInfo.pApplicationInfo := @appInfo;
      createInfo.enabledExtensionCount := glfwExtensionCount;
      createInfo.ppEnabledExtensionNames := @extensions[0];

      // Validation layers (solo in debug, molto utili!)
      if enableValidationLayers then
      begin
        createInfo.enabledLayerCount := Length(ValidationLayers);
        createInfo.ppEnabledLayerNames := @ValidationLayers[0];
      end
      else
      begin
        createInfo.enabledLayerCount := 0;
        createInfo.ppEnabledLayerNames := nil;
      end;

      // 5. Creazione vera e propria
      res := vkCreateInstance(@createInfo, nil, inst);
      if res <> VK_SUCCESS then
        raise Exception.CreateFmt('vkCreateInstance fallito con codice %d', [Ord(res)]);

    end;
    WriteLn('Vulkan Instance creato con successo!');
  end;

begin
  with getVulkan do
  begin
    try
      CreateVulkanInstance(instance);           // funzione helper - dovrai implementarla
      PickPhysicalDevice;
      CreateLogicalDevice;

      WriteLn('Tutto ok! Abbiamo un device logico.');

      vkDestroyDevice(device, nil);
      vkDestroyInstance(instance, nil);

    except
      on E: Exception do
      begin
        WriteLn('Errore: ', E.Message);
        Halt(1);
      end;
    end;
  end;

  ReadLn;
end.
