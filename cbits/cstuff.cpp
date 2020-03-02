#include <iostream>
#include <chrono>
#include <cstdlib>

#include <Audioclient.h>
#include <Audiopolicy.h>
#include <Mmdeviceapi.h>
#include <Combaseapi.h>
#include <math.h>

// REFERENCE_TIME time units per second and per millisecond
#define REFTIMES_PER_SEC  10'000'000
#define REFTIMES_PER_MILLISEC  10'000

#define EXIT_ON_ERROR(hres)  \
              if (FAILED(hres)) { std::cout << hres; goto Exit; }
#define SAFE_RELEASED(punk)  \
              if ((punk) != NULL)  \
                { (punk)->Release(); (punk) = NULL; }
#define SAFE_RELEASE(punk)

extern "C" void c_ReleaseAudio(IAudioClient* pAudioClient);
void c_ReleaseAudio(IAudioClient* pAudioClient){
  SAFE_RELEASE(pAudioClient);
}

extern "C" IAudioClient* c_InitAudio();
IAudioClient* c_InitAudio(){
  std::cout << "Initializing audio...\n";
  // Initialize the windows api stuff on this thread
  CoInitializeEx(NULL,COINIT_APARTMENTTHREADED | COINIT_SPEED_OVER_MEMORY);

  // Get a device enumerator to find the audio ("multi-media") devices
  IMMDeviceEnumerator *pEnumerator = NULL;
  CoCreateInstance(__uuidof(MMDeviceEnumerator), NULL,CLSCTX_ALL, __uuidof(IMMDeviceEnumerator),(void**)&pEnumerator);

  // Get the default audio renderer for console applications from the enumerator
  IMMDevice *pDevice = NULL;
  pEnumerator->GetDefaultAudioEndpoint(eRender, eConsole, &pDevice);
  SAFE_RELEASE(pEnumerator);

  // Create an audio client
  IAudioClient* pAudioClient = NULL;
  pDevice->Activate(__uuidof(IAudioClient), CLSCTX_ALL,NULL, (void**)&pAudioClient);
  SAFE_RELEASE(pDevice);

  // Initialize the audio client with its default, internal mix format (so we aren't resampling or anything)
  WAVEFORMATEX *pWaveFormat = NULL;
  pAudioClient->GetMixFormat(&pWaveFormat);

  pAudioClient->Initialize
    (AUDCLNT_SHAREMODE_SHARED // Shared mode; other things can also play audio, and we share a buffer with the audio engine, not the device
    ,0 // Stream flags (none)
    ,0 // Request minimum buffer duration. Actually nonzero and equal to audio engine speed
    ,0 // Can't request a different device period in shared mode
    ,pWaveFormat
    ,NULL // Not part of a greater audio session
    );

  // Get the actual size of the allocated buffer.
  UINT32 bufferFrameCount;
  pAudioClient->GetBufferSize(&bufferFrameCount);

  // Get the device frequency from the clock
  IAudioClock *pAudioClock = NULL;
  pAudioClient->GetService(__uuidof(IAudioClock),(void**)&pAudioClock);
  UINT64 devFreq = 0;
  pAudioClock->GetFrequency(&devFreq);
  SAFE_RELEASE(pAudioClock)

  // Log the audio format
  std::cout << "Bits per sample: " << std::dec << pWaveFormat->wBitsPerSample << "\n";
  std::cout << "Format Tag: 0x" << std::hex << pWaveFormat->wFormatTag << "\n";
  std::cout << "^ WAVE_FORMAT_PCM(0x" << std::hex << WAVE_FORMAT_PCM << ") or WAVE_FORMAT_EXTENSIBLE(0x" << std::hex << WAVE_FORMAT_EXTENSIBLE << ")\n";
  std::cout << "Sample Rate: " << std::dec << pWaveFormat->nSamplesPerSec << "\n";
  std::cout << "Channel Mask: 0x" << std::hex << ((WAVEFORMATEXTENSIBLE*) pWaveFormat)->dwChannelMask << "\n";
  std::cout << "SubFormat: 0x" << std::hex << ((WAVEFORMATEXTENSIBLE*) pWaveFormat)->SubFormat.Data1 << "\n";
  std::cout << "SubFormat: 0x" << std::hex << ((WAVEFORMATEXTENSIBLE*) pWaveFormat)->SubFormat.Data2 << "\n";
  std::cout << "SubFormat: 0x" << std::hex << ((WAVEFORMATEXTENSIBLE*) pWaveFormat)->SubFormat.Data3 << "\n";
  std::cout << "SubFormat: 0x" << std::hex << ((WAVEFORMATEXTENSIBLE*) pWaveFormat)->SubFormat.Data4 << "\n";
  std::cout << "Buffer Frame Count: " << std::dec << bufferFrameCount << "\n";
  std::cout << "Device Frequency: " << std::dec << devFreq << "\n";
  std::cout << "Device Frequency over Sample Rate: " << std::dec << (devFreq / (pWaveFormat -> nSamplesPerSec)) << "\n";

  //CoTaskMemFree(pWaveFormat);
  return pAudioClient;
}

typedef void* (*LoadCB)(UINT32,BYTE*,DWORD*);
typedef void* (*StartCoordCB)();

extern "C" void c_PlayAudioStream(IAudioClient* pAudioClient, LoadCB loadDataCallback, StartCoordCB sccb);
void c_PlayAudioStream(IAudioClient* pAudioClient, LoadCB loadDataCallback, StartCoordCB sccb){
    // Get the waveformat
    WAVEFORMATEX *pWaveFormat = NULL;
    pAudioClient->GetMixFormat(&pWaveFormat);

    // Get the actual size of the allocated buffer
    UINT32 bufferFrameCount;
    pAudioClient->GetBufferSize(&bufferFrameCount);

    // Get the render client
    IAudioRenderClient *pRenderClient = NULL;
    pAudioClient->GetService(__uuidof(IAudioRenderClient),(void**)&pRenderClient);

    // Grab the entire buffer for the initial fill operation
    BYTE *pRenderBuffer;
    pRenderClient->GetBuffer(bufferFrameCount, &pRenderBuffer);

    // Load the initial data into the render buffer
    DWORD flags = 0;
    (*loadDataCallback)(bufferFrameCount, pRenderBuffer, &flags);

    pRenderClient->ReleaseBuffer(bufferFrameCount, flags);

    // Calculate the amount of time to sleep between requesting data as half the buffer length, in millis. Reference time is in 100 nano second units.
    DWORD slumberTime = 1000 * (bufferFrameCount / pWaveFormat->nSamplesPerSec) / 2;
    //CoTaskMemFree(pWaveFormat);

    // Call the start coordination callback!
    if(!(sccb == NULL)){
      (*sccb)();
      sccb = NULL;
    }

    // Start playing
    pAudioClient->Start();

    UINT32 numFramesAvailable;
    UINT32 numFramesPadding;
    // Keep filling buffer while there is more data
    while(flags != AUDCLNT_BUFFERFLAGS_SILENT){
      Sleep(slumberTime);

      // See how full the buffer is
      pAudioClient->GetCurrentPadding(&numFramesPadding);

      // Calculate how much more we can put in the buffer. Should be about 50% because we sleep for half the duration.
      numFramesAvailable = bufferFrameCount - numFramesPadding;

      // Grab all the available space in the buffer
      pRenderClient->GetBuffer(numFramesAvailable, &pRenderBuffer);
      //auto start = std::chrono::high_resolution_clock::now();
      // Fill the available space in the buffer using the callback
      (*loadDataCallback)(numFramesAvailable, pRenderBuffer, &flags);
      //auto stop = std::chrono::high_resolution_clock::now();
      //auto duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start); 
  
      //std::cout << "Timing: " << duration.count() << std::endl;
      
      // Return the now full buffer to the audio engine
      pRenderClient->ReleaseBuffer(numFramesAvailable, flags);
    }

    // Wait for last data in buffer to play before stopping
    Sleep(slumberTime);

    // Stop playing
    pAudioClient->Stop();
    SAFE_RELEASE(pAudioClient)
    SAFE_RELEASE(pRenderClient)
    std::cout << "\n\nExiting cbits audio stream\n\n";
}
