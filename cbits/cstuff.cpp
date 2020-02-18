// #include <windows.h>
// #include <libloaderapi.h>
// #include <vector>
// #include <iostream>
// #include <csignal>
// #include <plugin.hpp>
// #include <Plugin.hpp>

#include <iostream>
#include <cstdlib>

//-----------------------------------------------------------
// Play an audio stream on the default audio rendering
// device. The PlayAudioStream function allocates a shared
// buffer big enough to hold one second of PCM audio data.
// The function uses this buffer to stream data to the
// rendering device. The inner loop runs every 1/2 second.
//-----------------------------------------------------------

#include <Audioclient.h>
#include <Audiopolicy.h>
#include <Mmdeviceapi.h>
#include <Combaseapi.h>
#include <math.h>

// REFERENCE_TIME time units per second and per millisecond
#define REFTIMES_PER_SEC  10000000
#define REFTIMES_PER_MILLISEC  10000

#define EXIT_ON_ERROR(hres)  \
              if (FAILED(hres)) { std::cout << hres; goto Exit; }
#define SAFE_RELEASE(punk)  \
              if ((punk) != NULL)  \
                { (punk)->Release(); (punk) = NULL; }


const CLSID CLSID_MMDeviceEnumerator = __uuidof(MMDeviceEnumerator);
const IID IID_IMMDeviceEnumerator = __uuidof(IMMDeviceEnumerator);
const IID IID_IAudioClient = __uuidof(IAudioClient);
const IID IID_IAudioClock = __uuidof(IAudioClock);
const IID IID_IAudioRenderClient = __uuidof(IAudioRenderClient);


float* preppedWave = NULL; 



float phase = 0;
int loads = 0;
extern "C" int sinWaveLDC(UINT32 samplesToBuffer,BYTE* ptr, DWORD* flags);
int sinWaveLDC(UINT32 samplesToBuffer,BYTE* ptr, DWORD* flags){
  loads++;
  int periods = 0;
  for(int i = 0; i < samplesToBuffer; i++){
    phase += 0.1f;
    if(phase > 6.28){
      phase -= 6.28;
      periods++;
    }
    ((float *)ptr)[2*i] = sin(phase);
    ((float *)ptr)[2*i+1] = sin(phase);
  }
  //std::cout << "Loaded in at least " << periods << " periods!\n";
  *flags = 0;
  return (loads > 10);
}

typedef void* (*LoadCB)(UINT32,BYTE*,DWORD*);
typedef void* (*StartCoordCB)(UINT64);

extern "C" HRESULT PlayAudioStream(LoadCB loadDataCallback, StartCoordCB sccb);
HRESULT PlayAudioStream(LoadCB loadDataCallback, StartCoordCB sccb)//void* (*loadDataCallback)(UINT32,BYTE*,DWORD*))
{
    HRESULT hr;
    REFERENCE_TIME hnsRequestedDuration = REFTIMES_PER_SEC;
    REFERENCE_TIME hnsActualDuration;
    IMMDeviceEnumerator *pEnumerator = NULL;
    IMMDevice *pDevice = NULL;
    IAudioClient *pAudioClient = NULL;
    IAudioRenderClient *pRenderClient = NULL;
    IAudioClock *pAudioClock = NULL;
    WAVEFORMATEX *pwfx = NULL;
    UINT32 bufferFrameCount;
    UINT32 numFramesAvailable;
    UINT32 numFramesPadding;
    BYTE *pData;
    DWORD flags = 0;
    int killAudioStream = 0;
    UINT64 checkPos = 0;
    UINT64 devFreq = 0;

    hr = CoInitialize( NULL );
    EXIT_ON_ERROR(hr)

    hr = CoCreateInstance(
           CLSID_MMDeviceEnumerator, NULL,
           CLSCTX_ALL, IID_IMMDeviceEnumerator,
           (void**)&pEnumerator);
    EXIT_ON_ERROR(hr)

    hr = pEnumerator->GetDefaultAudioEndpoint(
                        eRender, eConsole, &pDevice);
    EXIT_ON_ERROR(hr)

    //std::cout << "Activate ...";
    hr = pDevice->Activate(
                    IID_IAudioClient, CLSCTX_ALL,
                    NULL, (void**)&pAudioClient);
    EXIT_ON_ERROR(hr)

    hr = pAudioClient->GetMixFormat(&pwfx);
    EXIT_ON_ERROR(hr)

    hr = pAudioClient->Initialize(
                         AUDCLNT_SHAREMODE_SHARED,
                         0,
                         hnsRequestedDuration,
                         0,
                         pwfx,
                         NULL);
    EXIT_ON_ERROR(hr)

    // Tell the audio source which format to use.
    //std::cout << "Bits: " << pwfx->wBitsPerSample << "\n";
    //std::cout << "Format Tag: " << pwfx->wFormatTag << "\n";
    //std::cout << "Format Tag Options: WAVE_FORMAT_PCM " << WAVE_FORMAT_PCM << "; WAVE_FORMAT_EXTENSIBLE" << WAVE_FORMAT_EXTENSIBLE << "\n";
    //std::cout << "Sample Rate: " << pwfx->nSamplesPerSec << "\n";
    //std::cout << "Channel Mask: " << ((WAVEFORMATEXTENSIBLE*) pwfx)->dwChannelMask << "\n";
    //std::cout << "SubFormat: " << ((WAVEFORMATEXTENSIBLE*) pwfx)->SubFormat.Data1 << "\n";
    //std::cout << "SubFormat: " << ((WAVEFORMATEXTENSIBLE*) pwfx)->SubFormat.Data2 << "\n";
    //std::cout << "SubFormat: " << ((WAVEFORMATEXTENSIBLE*) pwfx)->SubFormat.Data3 << "\n";
    //std::cout << "SubFormat: " << ((WAVEFORMATEXTENSIBLE*) pwfx)->SubFormat.Data4 << "\n";
    //hr = pMySource->SetFormat(pwfx);
    //EXIT_ON_ERROR(hr)

    // Get the actual size of the allocated buffer.
    hr = pAudioClient->GetBufferSize(&bufferFrameCount);
    EXIT_ON_ERROR(hr)

    hr = pAudioClient->GetService(
                         IID_IAudioRenderClient,
                         (void**)&pRenderClient);
    EXIT_ON_ERROR(hr)

    hr = pAudioClient->GetService(
                         IID_IAudioClock,
                         (void**)&pAudioClock);
    EXIT_ON_ERROR(hr)

    pAudioClock->GetFrequency(&devFreq);
    //std::cout << "GetFrequency: " << devFreq << "\n";

    // Grab the entire buffer for the initial fill operation.
    hr = pRenderClient->GetBuffer(bufferFrameCount, &pData);
    EXIT_ON_ERROR(hr)

    // Load the initial data into the shared buffer.
    //std::cout << "Loading initial data...\n";
    loadDataCallback = (LoadCB) (*loadDataCallback)(bufferFrameCount, pData, &flags);
    EXIT_ON_ERROR(hr)
    //std::cout << "Some data: " << (float)(*pData);

    hr = pRenderClient->ReleaseBuffer(bufferFrameCount, flags);
    EXIT_ON_ERROR(hr)

    // Calculate the actual duration of the allocated buffer.
    hnsActualDuration = (double)REFTIMES_PER_SEC *
                        bufferFrameCount / pwfx->nSamplesPerSec;

    hr = pAudioClient->Start();  // Start playing.
    EXIT_ON_ERROR(hr)

    // Each loop fills about half of the shared buffer.
    while (!(loadDataCallback == NULL) && flags != AUDCLNT_BUFFERFLAGS_SILENT)
    {
        // See how much buffer space is available.
        hr = pAudioClient->GetCurrentPadding(&numFramesPadding);
        EXIT_ON_ERROR(hr)
        //std::cout << "Padding before is: " << numFramesPadding << "\n";

        pAudioClock->GetPosition(&checkPos,NULL);
        //std::cout << "Position Before: " << checkPos << "\n";
        //UINT64 before = checkPos;
        // Sleep for half the buffer duration.
        Sleep((DWORD)(hnsActualDuration/REFTIMES_PER_MILLISEC/2));
        pAudioClock->GetPosition(&checkPos,NULL);
        if(!(sccb == NULL)){
          (*sccb)(checkPos);
          sccb = NULL;
        }
        //std::cout << "Position After: " << checkPos << "\n";
        //std::cout << "Position Delta: " << (checkPos - before) << "\n\n";

        // See how much buffer space is available.
        hr = pAudioClient->GetCurrentPadding(&numFramesPadding);
        EXIT_ON_ERROR(hr)
        //std::cout << "Padding after is: " << numFramesPadding << "\n";

        numFramesAvailable = bufferFrameCount - numFramesPadding;

        // Grab all the available space in the shared buffer.
        //std::cout << "Getting " << numFramesAvailable << " frames due to frame count " << bufferFrameCount << "\n\n";
        hr = pRenderClient->GetBuffer(numFramesAvailable, &pData);
        EXIT_ON_ERROR(hr)

        //std::cout << "Calling load data callback!";
        // Get next 1/2-second of data from the audio source.
        loadDataCallback = (LoadCB) (*loadDataCallback)(numFramesAvailable, pData, &flags);
        //std::cout << "Phase is: " << phase;
        //std::cout << "First data point is: " << (float)(((float*)pData)[0]) << "\n";
        //std::cout << "Last data point is: " << (float)(((float*)pData)[numFramesAvailable - 1]) << "\n";

        //std::cout << "Releasing buffer...";
        hr = pRenderClient->ReleaseBuffer(numFramesAvailable, flags);
        EXIT_ON_ERROR(hr)
    }
    //std::cout << "Exit flag set!";

    // Wait for last data in buffer to play before stopping.
    Sleep((DWORD)(hnsActualDuration/REFTIMES_PER_MILLISEC/2));

    hr = pAudioClient->Stop();  // Stop playing.
    EXIT_ON_ERROR(hr)

Exit:
    CoTaskMemFree(pwfx);
    SAFE_RELEASE(pEnumerator)
    SAFE_RELEASE(pDevice)
    SAFE_RELEASE(pAudioClient)
    SAFE_RELEASE(pRenderClient)
    std::cout << "\n\nExiting cbits audio stream\n\n";

    return hr;
}

/*
extern "C" long long hs_GetDataPointer(rack::plugin::Plugin* plugin);
long long hs_GetDataPointer(rack::plugin::Plugin* plugin){
  std::cout << '[';
  for (auto&& i : plugin->models) std::cout << 'x';
  std::cout << ']';
  long long x = (long long) (plugin -> models.data());
  return x;
}


typedef void (*InitCallback)(rack::plugin::Plugin*);

extern "C" rack::plugin::Plugin* hs_LoadRackPlugin(const char *plugin);
rack::plugin::Plugin* hs_LoadRackPlugin(const char *plugin){
  typedef DLL_DIRECTORY_COOKIE(WINAPI * PAddDllDirectory)(PCWSTR);
  typedef BOOL(WINAPI * PSetDefaultDllDirectories)(DWORD);

  PAddDllDirectory add_dll_directory = (PAddDllDirectory)GetProcAddress(GetModuleHandle("kernel32.dll"), "AddDllDirectory");
  PSetDefaultDllDirectories set_default_dll_directories = (PSetDefaultDllDirectories)GetProcAddress(GetModuleHandle("kernel32.dll"), "SetDefaultDllDirectories");

  //std::raise(SIGINT);
  SetErrorMode(0x8001);
  set_default_dll_directories(0x1700);
  //add_dll_directory(L"C:\\ProgramFiles\\VCV\\Rack");
  add_dll_directory(L"C:\\Users\\Sam\\Documents\\GitHub\\boopadoop\\cbits\\");
  //add_dll_directory(L"C:\\Users\\Sam\\Documents\\GitHub\\boopadoop\\plugin\\");
  add_dll_directory(L"C:\\Users\\Sam\\Documents\\GitHub\\boopadoop\\");
  HINSTANCE handle = LoadLibrary("C:\\Users\\Sam\\Documents\\GitHub\\boopadoop\\plugin.dll");
  SetErrorMode(0);
  if (!handle) {
    int error = GetLastError();
    std::cout << "Error loading plugin: " << error;
  }
  rack::plugin::Plugin* thePlugin = new rack::plugin::Plugin;
  //memset((void*)plugin,0xdead,1024);
  thePlugin->handle = handle;
  thePlugin->path = "plugin";

  return thePlugin;
}
extern "C" InitCallback hs_GetInitCallback(rack::plugin::Plugin* plugin);
InitCallback hs_GetInitCallback(rack::plugin::Plugin* plugin){
  std::cout << "Plugin path is: " << plugin->path;

  //rack::plugin::Model* fakeModel = new rack::plugin::Model;
  //fakeModel->plugin = plugin;
  //plugin->models.push_back(fakeModel);
  plugin->models.push_back(NULL);
  std::cout << "data pointer is: " << (long long) plugin->models.data() << "\n";
  
  InitCallback initCallback = (InitCallback) GetProcAddress((HINSTANCE)(plugin->handle),"init");
  if (!initCallback) {
    std::cout << "Null init callback!\n";
  }
  //initCallback(plugin);
  //rack::plugin::init();
  return initCallback;
}
*/

/*
extern "C" rack::plugin::Model* hs_GetModelBySlug(rack::plugin::Plugin* plugin, const char *slug);
rack::plugin::Model* hs_GetModelBySlug(rack::plugin::Plugin* plugin, const char *slug){
  return plugin->getModel(slug);
}
*/


/*
int main(){

  // Windows global mutex to prevent running at the same time as actual Rack
  // Handle will be closed by Windows when the process ends
  HANDLE instanceMutex = CreateMutexA(NULL, true, "VCV Rack");
  if (GetLastError() == ERROR_ALREADY_EXISTS) {
    osdialog_message(OSDIALOG_ERROR, OSDIALOG_OK, "Rack is already running. Can't use Boopadoop rack support and Rack at the same time.");
    exit(1);
  }
  (void) instanceMutex;

  std::cout << "hi\n";
  rack::plugin::Plugin* plugin = hs_LoadRackPlugin("plugin/plugin.dll");
  std::cout << "still alive1\n";
  InitCallback cb = hs_GetInitCallback(plugin);
  std::cout << "still alive5\n";

  rack::asset::init();
  std::cout << "still alive18\n";
  rack::logger::init(); 
  std::cout << "still alive14\n";

  rack::plugin::init();


  //cb(plugin);
  std::cout << "still alive2\n";
}
*/


/*int tableLDC(UINT32 samplesToBuffer,BYTE* ptr, DWORD* flags){
  memcpy((float*)ptr,preppedWave,samplesToBuffer * sizeof(float));
  loads++;
  return (loads > 15);
}

extern "C" void runFast();
void runFast(){
  preppedWave = (float*) malloc(50000 * sizeof(float));
  for(int i = 0; i < 50000; i++){
    phase += 0.1f;
    if(phase > 6.28){
      phase -= 6.28;
    }
    ((float *)preppedWave)[i] = sin(phase);
  }
  PlayAudioStream(&sinWaveLDC);
}*/
