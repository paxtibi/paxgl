program vulkanglfwexample;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  pax.glfw,
  pax.vulkan;

var
  window: PGLFWwindow;
  instance: VkInstance;
  createInfo: VkInstanceCreateInfo;
  appInfo: VkApplicationInfo;
  Result: VkResult;

  procedure ErrorCallback(err: integer; const description: pansichar); cdecl;
  begin
    WriteLn('GLFW Error ', err, ': ', description);
  end;

  function CheckVulkanSupport: boolean;
  var
    glfwExtensions: PPAnsiChar;
    glfwExtensionCount: uint32;
    i: integer;
  begin
    with getGLFW do
    begin
      Result := False;

      if not glfwVulkanSupported() then
      begin
        WriteLn('GLFW: Vulkan non supportato!');
        Exit;
      end;

      glfwExtensions := glfwGetRequiredInstanceExtensions(glfwExtensionCount);
      if glfwExtensionCount = 0 then
      begin
        WriteLn('GLFW: Nessuna estensione richiesta restituita');
        Exit;
      end;

      WriteLn('Estensioni richieste da GLFW (', glfwExtensionCount, '):');
      for i := 0 to glfwExtensionCount - 1 do
        WriteLn('  ', glfwExtensions[i]);

      Result := True;

    end;
  end;

var
  pAllocator: PVkAllocationCallbacks = nil;
begin
  with getGLFW, getVulkan do
  begin
    // Inizializzazione GLFW
    if not glfwInit then
    begin
      WriteLn('Errore inizializzazione GLFW');
      Exit;
    end;

    glfwSetErrorCallback(@ErrorCallback);

    glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
    glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);

    window := glfwCreateWindow(1280, 720, 'Vulkan + GLFW - Esempio base', nil, nil);
    if window = nil then
    begin
      glfwTerminate;
      WriteLn('Impossibile creare finestra GLFW');
      Exit;
    end;

    WriteLn('Finestra GLFW creata con successo');

    // Verifica supporto Vulkan tramite GLFW
    if not CheckVulkanSupport then
    begin
      glfwDestroyWindow(window);
      glfwTerminate;
      Exit;
    end;

    // ==============================================
    // Creazione istanza Vulkan molto basilare
    // ==============================================

    FillChar(appInfo, SizeOf(appInfo), 0);
    appInfo.sType := VK_STRUCTURE_TYPE_APPLICATION_INFO;
    appInfo.pApplicationName := 'Esempio GLFW+Vulkan';
    appInfo.applicationVersion := VK_MAKE_VERSION(1, 0, 0);
    appInfo.pEngineName := 'Nessun engine';
    appInfo.engineVersion := VK_MAKE_VERSION(1, 0, 0);
    appInfo.apiVersion := VK_API_VERSION_1_0;

    FillChar(createInfo, SizeOf(createInfo), 0);
    createInfo.sType := VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
    createInfo.pApplicationInfo := @appInfo;

    // Estensioni richieste da GLFW
    createInfo.ppEnabledExtensionNames := glfwGetRequiredInstanceExtensions(createInfo.enabledExtensionCount);
    createInfo.ppEnabledExtensionNames := glfwGetRequiredInstanceExtensions(createInfo.enabledExtensionCount);

    // Layers (esempio: validation layer - commentare in release)
    // createInfo.enabledLayerCount := 1;
    // createInfo.ppEnabledLayerNames := @VALIDATION_LAYER_NAME;

    Result := vkCreateInstance(@createInfo, nil, instance);
    if Result <> VK_SUCCESS then
    begin
      WriteLn('Errore creazione istanza Vulkan: ', Ord(Result));
      glfwDestroyWindow(window);
      glfwTerminate;
      Exit;
    end;

    WriteLn('Istanza Vulkan creata con successo!');

    // ==============================================
    // Ciclo principale
    // ==============================================

    while not glfwWindowShouldClose(window) do
    begin
      glfwPollEvents;
    end;

    try
      vkDestroyInstance(instance, pAllocator);
    except

    end;
    glfwDestroyWindow(window);
    glfwTerminate;

    WriteLn('Applicazione terminata correttamente.');
  end;
end.
