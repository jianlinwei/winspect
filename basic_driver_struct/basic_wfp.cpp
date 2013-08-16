#include "stdafx.h"

// ---------------------------
extern "C"
{
#include <ndis.h>
#include <fwpsk.h>
#include <fwpmk.h>
#include <guiddef.h>
#include "fwpvi.h"
#include "fwpstypes.h"
};

// ---------------------------

#ifdef __cplusplus
extern "C" NTSTATUS DriverEntry(IN PDRIVER_OBJECT DriverObject, IN PUNICODE_STRING  RegistryPath);
#endif

// ------------------------------ Автогенерированные объявления функций -------------

void basic_wfpUnload(IN PDRIVER_OBJECT DriverObject);
NTSTATUS basic_wfpCreateClose(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp);
NTSTATUS basic_wfpDefaultHandler(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp);
NTSTATUS basic_wfpAddDevice(IN PDRIVER_OBJECT  DriverObject, IN PDEVICE_OBJECT  PhysicalDeviceObject);
NTSTATUS basic_wfpPnP(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp);

// ------------------------------ Объявления кастомных функций -----------------------
//

NTSTATUS RegisterCalloutForLayer(                             // Функция регистрации коллаута для определенного слоя (layer).
	IN const GUID* layerKey, 
	IN const GUID* calloutKey, 
	IN void* deviceObject, 
	OUT UINT32* calloutId);
																																	

void NTAPI TrafficBasicClassifyFn(
	IN const FWPS_INCOMING_VALUES* inFixedValues,
	IN const FWPS_INCOMING_METADATA_VALUES* inMetaValues,
	IN OUT void* layerData,
	IN const void* classifyContext,
	IN const FWPS_FILTER* filter,
	IN UINT64 flowContext,
	OUT FWPS_CLASSIFY_OUT* classifyOut
	);

NTSTATUS NTAPI TrafficBasicNotifyFn(                                // Функция-нотификатор. Не используется, но должна быть определена.
	IN FWPS_CALLOUT_NOTIFY_TYPE notifyType,
	IN const GUID* filterKey,
	IN const FWPS_FILTER* filter);

NTSTATUS RegisterCallout(
	IN const PVOID gUnref,             
	IN void* deviceObject);

void UnregisterCallout();

// ------------------ Область объявления глобальных переменных -----------------------
//

typedef struct _deviceExtension
{
	PDEVICE_OBJECT DeviceObject;
	PDEVICE_OBJECT TargetDeviceObject;
	PDEVICE_OBJECT PhysicalDeviceObject;
	UNICODE_STRING DeviceInterface;
} basic_wfp_DEVICE_EXTENSION, *Pbasic_wfp_DEVICE_EXTENSION;

// {302a85b1-44b2-49b7-8909-e45262d32584} автоматически сгенерированный GUID, можно попользовать uuidgen для этих целей.
static const GUID GUID_basic_wfpInterface = {0x302A85B1, 0x44b2, 0x49b7, {0x89, 0x9, 0xe4, 0x52, 0x62, 0xd3, 0x25, 0x84 } };

// GUIDы коллаутов.

static const GUID DRIVER_STREAM_CALLOUT_V4 = {0xe6011cdc,
	0x440b,
	0x4a6f,
	{0x84, 0x99, 0x6f, 0xdb, 0x55, 0xfb, 0x1f, 0x92}};

static const GUID DRIVER_STREAM_CALLOUT_V6 = {0xc0bc07b4,
	0xaaf6,
	0x4242,
	{0xa3, 0xdc, 0x3e, 0xf3, 0x41, 0xff, 0xde, 0x5d}};


const bool gDBG_Mode = true;            // Включение режима отладочной печати.

#define DRIVER_NDIS_OBJ_TAG 'oneS' // Тэг в пространстве памяти ядра для экземпляра Generic NDIS.
#define DRIVER_NBL_POOL_TAG 'pneS'

const USHORT configInspectionPort = 0;      // Порт, который будет инспектироваться.
const bool packetDatatToString = TRUE;

PDEVICE_OBJECT gDeviceObject;           // Generic Device Object :-)
PNDIS_GENERIC_OBJECT gNdisGenericObj;   // Экземпляр устройства NDIS.
HANDLE gEngineHandle;                   // Хэндл движка WFP.
NDIS_HANDLE gNetBufferListPool;
UINT32 gCalloutIdV6, gCalloutIdV4;

// -----------------------------------------------------------------------------------
//                           Точка входа в драйвер

NTSTATUS DriverEntry(IN PDRIVER_OBJECT DriverObject, IN PUNICODE_STRING  RegistryPath)
{
	
	// Эта часть текста сгенерирована VisualDDK
	unsigned i;
	if(gDBG_Mode) DbgPrint("Basic WFP test driver loaded.\n");
	
	// Инициализация полей DriverObject
	for (i = 0; i <= IRP_MJ_MAXIMUM_FUNCTION; i++)
		DriverObject->MajorFunction[i] = basic_wfpDefaultHandler;

	DriverObject->MajorFunction[IRP_MJ_CREATE] = basic_wfpCreateClose; // Заполнение
	DriverObject->MajorFunction[IRP_MJ_CLOSE] = basic_wfpCreateClose;  // структуры
	DriverObject->MajorFunction[IRP_MJ_PNP] = basic_wfpPnP;            // DriverObject

	DriverObject->DriverUnload = basic_wfpUnload; // Указатель на функцию выгрузки драйвера.
	DriverObject->DriverStartIo = NULL;
	DriverObject->DriverExtension->AddDevice = basic_wfpAddDevice; // Для драйвера в системе создается фиктивное устройство класса NetTrans
	// Конец автогенерированной части ---

	NTSTATUS opStatus = STATUS_SUCCESS;
	UNICODE_STRING deviceName;
	NET_BUFFER_LIST_POOL_PARAMETERS nblPoolParams = {0}; // Глюк студии? - для компиляции требуется указать -DNDIS60 -DNDIS_SUPPORT_NDIS6 в sources.
														 // Так как разрабатываем для NT > 5.2, то, по фен-шую, требуется использовать структуры NDIS 6.0
	                                                     // для хранения данных пакетов.

	RtlInitUnicodeString(&deviceName, L"\\Device\\basic_wfp");

	opStatus = IoCreateDevice(DriverObject, // Настоящий экземпляр DriverObject 
			   0,                           // Пространство в памяти для расширения объекта драйвера - не используется здесь.
               &deviceName,                 // Имя устройства (с путем включительно, для создания underlying tree).
               FILE_DEVICE_NETWORK,         // Тип устройства 0x00000012.
               0,                           // DeviceCharacteristics - не используется.
               FALSE,                       // EXCLUSIVE = FALSE - с разделяемым доступом.
               &gDeviceObject);              // Указатель на созданную DEVICE_OBJECT структуру.
               
	if (!NT_SUCCESS(opStatus)) goto Exit;   // Если объект не создался -- то у нас проблемы и нужно освобождать ресурсы и выгружать драйвер.

	gNdisGenericObj = NdisAllocateGenericObject(
                        DriverObject,                  // Экземпляр DriverObject, ассоциирующийся с GenericNDIS
                        DRIVER_NDIS_OBJ_TAG,           // Тэг в пространстве ядреной памяти, с которым связан NDIS object
                        0);							   // Размер памяти, резевируемый для вызывающей функции.

	if (gNdisGenericObj == NULL)
	{
		opStatus = STATUS_NO_MEMORY;
		goto Exit;
	}

	nblPoolParams.Header.Type = NDIS_OBJECT_TYPE_DEFAULT;
	nblPoolParams.Header.Revision = NET_BUFFER_LIST_POOL_PARAMETERS_REVISION_1;
	nblPoolParams.Header.Size = sizeof(nblPoolParams);

	nblPoolParams.fAllocateNetBuffer = TRUE;
	nblPoolParams.DataSize = 0;

	nblPoolParams.PoolTag = DRIVER_NBL_POOL_TAG;

	gNetBufferListPool = NdisAllocateNetBufferListPool(
		gNdisGenericObj,
		&nblPoolParams);

	if (gNetBufferListPool == NULL)
	{
		opStatus = STATUS_NO_MEMORY;
		goto Exit;
	}

	opStatus = RegisterCallout(
		NULL,
		gDeviceObject);

	if (!NT_SUCCESS(opStatus))
	{
		goto Exit;
	}


	Exit:
	
	if (!NT_SUCCESS(opStatus))
	{
		if (gEngineHandle != NULL) UnregisterCallout();
		if (gNetBufferListPool != NULL)	NdisFreeNetBufferListPool(gNetBufferListPool);
		if (gNdisGenericObj != NULL) NdisFreeGenericObject(gNdisGenericObj);
		if (gDeviceObject) IoDeleteDevice(gDeviceObject); // Подчищаем за собой ресурсы в случае неудачи создания экземпляра устройства.
	}

	return opStatus;
}

// ------------------------------------------------------------------------------------
//                          Функция выгрузки драйвера
// ------------------------------------------------------------------------------------
void basic_wfpUnload(IN PDRIVER_OBJECT DriverObject)
{
	if(gDBG_Mode) DbgPrint("Basic WFP test driver unloaded.\n");
}

// ---------------------------------------------------------------------------------------
// Функция, регистрирующая коллаут для соответствующего слоя.

NTSTATUS RegisterCalloutForLayer(
   IN const GUID* layerKey,       // GUID слоя, на который будет "навешен" callout.
   IN const GUID* calloutKey,
   IN void* deviceObject,
   OUT UINT32* calloutId)
{
   NTSTATUS status = STATUS_SUCCESS;

   FWPS_CALLOUT sCallout = {0}; // Структура коллаута. См. MSDN FWPS_CALLOUT0

   FWPM_FILTER filter = {0};
   FWPM_FILTER_CONDITION filterConditions[1] = {0}; 

   FWPM_CALLOUT mCallout = {0}; // Структура состояния коллаута.
   FWPM_DISPLAY_DATA displayData = {0};

   BOOLEAN calloutRegistered = FALSE;

   sCallout.calloutKey = *calloutKey;            // GUID коллаута.

   sCallout.classifyFn = TrafficBasicClassifyFn;
   sCallout.notifyFn = (FWPS_CALLOUT_NOTIFY_FN1) TrafficBasicNotifyFn;

   status = FwpsCalloutRegister(                 // Собственно, регистрация коллаута.
               deviceObject,
               &sCallout,
               calloutId);

   if (!NT_SUCCESS(status))
   {
      goto Exit;
   }

   calloutRegistered = TRUE;

   displayData.name = L"Basic Stream Inspection Callout";
   displayData.description = L"Callout that sniffs TCP stream :)";

   mCallout.calloutKey = *calloutKey;
   mCallout.displayData = displayData;
   mCallout.applicableLayer = *layerKey;
  
   status = FwpmCalloutAdd(
               gEngineHandle,
               &mCallout,
               NULL,
               NULL
               );

   if (!NT_SUCCESS(status))
   {
      goto Exit;
   }

   filter.layerKey = *layerKey;
   filter.displayData.name = L"Stream inspection Filter";
   filter.displayData.description = L"Filter that inspects TCP stream";

   filter.action.type = FWP_ACTION_CALLOUT_TERMINATING;
   filter.action.calloutKey = *calloutKey;
   filter.filterCondition = filterConditions;           // Т.к. мы ничего не отфильтровываем, а просто смотрим TCP stream, то здесь 0.
   filter.numFilterConditions = 1;
   filter.subLayerKey = FWPM_SUBLAYER_UNIVERSAL;
   filter.weight.type = FWP_EMPTY;                      // auto-weight.

   filterConditions[0].fieldKey =  FWPM_CONDITION_IP_REMOTE_PORT;
   filterConditions[0].matchType = FWP_MATCH_GREATER_OR_EQUAL;
   filterConditions[0].conditionValue.type = FWP_UINT16;
   filterConditions[0].conditionValue.uint16 = configInspectionPort; // Порт, с которого будем начинать смотреть.

   status = FwpmFilterAdd(
               gEngineHandle,
               &filter,
               NULL,
               NULL);
   if (!NT_SUCCESS(status))
   {
      if(gDBG_Mode) DbgPrint("Error while adding filter %d",status);
	  goto Exit;
   }

   
Exit:

   if (!NT_SUCCESS(status))
   {
      if (calloutRegistered)
      {
         if(gDBG_Mode) DbgPrint("Error while adding filter %d",status);
		 FwpsCalloutUnregisterById(*calloutId);
      }
   }

   return status;
}

// -------------------------- Функция регистрации коллаута ----------------------------------
//

NTSTATUS RegisterCallout(
   IN const PVOID gUnref,              
   IN void* deviceObject)
{
   NTSTATUS status = STATUS_SUCCESS;

   BOOLEAN engineOpened = FALSE;
   BOOLEAN inTransaction = FALSE;

   FWPM_SESSION session = {0};

   //UNREFERENCED_PARAMETER(streamEditor);

   session.flags = FWPM_SESSION_FLAG_DYNAMIC;

   status = FwpmEngineOpen(
                NULL,
                RPC_C_AUTHN_WINNT,
                NULL,
                &session,
                &gEngineHandle
                );
   if (!NT_SUCCESS(status))
   {
      goto Exit;
   }
   engineOpened = TRUE;

   status = FwpmTransactionBegin(gEngineHandle, 0);
   if (!NT_SUCCESS(status))
   {
      goto Exit;
   }
   inTransaction = TRUE;

   status = RegisterCalloutForLayer(
               &FWPM_LAYER_STREAM_V4,
               &DRIVER_STREAM_CALLOUT_V4,
               deviceObject,
               &gCalloutIdV4
               );
   if (!NT_SUCCESS(status))
   {
      if(gDBG_Mode) DbgPrint("Error while registering V4 callout %d",status);
	  goto Exit;
   }
   
   if(gDBG_Mode) DbgPrint("TCP/IP V4 Callout registered\n");
   
   status = RegisterCalloutForLayer(
               &FWPM_LAYER_STREAM_V6,
               &DRIVER_STREAM_CALLOUT_V6,
               deviceObject,
               &gCalloutIdV6
               );

   if(gDBG_Mode) DbgPrint("TCP/IP V6 Callout registered\n");

   if (!NT_SUCCESS(status))
   {
	   if(gDBG_Mode) DbgPrint("Error while registering V6 callout %d",status);
	   goto Exit;
   }
     

   status = FwpmTransactionCommit(gEngineHandle);
   if (!NT_SUCCESS(status))
   {
      goto Exit;
   }
   inTransaction = FALSE;

   if(gDBG_Mode) DbgPrint("WFP transaction commited\n");
Exit:

   if (!NT_SUCCESS(status))
   {
      if(gDBG_Mode) DbgPrint("Error while registering callouts\n");
	  if (inTransaction)
      {
         FwpmTransactionAbort(gEngineHandle);
      }
      if (engineOpened)
      {
         FwpmEngineClose(gEngineHandle);
         gEngineHandle = NULL;
      }
   }

   return status;
}

// -------------------------------------------------------------------------------------
// Функция разрегистрации коллаутов

void UnregisterCallout()
{
	FwpmEngineClose(gEngineHandle);
	gEngineHandle = NULL;

	FwpsCalloutUnregisterById(gCalloutIdV6);
	FwpsCalloutUnregisterById(gCalloutIdV4);
}

// -------------------------------------------------------------------------------------
// Функция-классификатор

void NTAPI TrafficBasicClassifyFn(
	IN const FWPS_INCOMING_VALUES* inFixedValues,
	IN const FWPS_INCOMING_METADATA_VALUES* inMetaValues,
	IN OUT void* layerData,
	IN const void* classifyContext,
	IN const FWPS_FILTER* filter,
	IN UINT64 flowContext,
	OUT FWPS_CLASSIFY_OUT* classifyOut
	)
{
	FWPS_STREAM_CALLOUT_IO_PACKET* ioPacket;
	FWPS_STREAM_DATA* streamData;

	ioPacket = (FWPS_STREAM_CALLOUT_IO_PACKET*) layerData;
	ASSERT(ioPacket != NULL);

	streamData = ioPacket->streamData;
	ASSERT(streamData != NULL);
	
	/*
	if ((configInspectionOutbound  && (streamData->flags & FWPS_STREAM_FLAG_RECEIVE)) ||
		(!configInspectionOutbound && (streamData->flags & FWPS_STREAM_FLAG_SEND)))
	{
		ioPacket->streamAction = FWPS_STREAM_ACTION_NONE;
		classifyOut->actionType = FWP_ACTION_PERMIT;
		goto Exit;
	}
	*/
	
	
	PBYTE packetData;
	SIZE_T bytesCopied;
	SIZE_T dataLength = streamData->dataLength;
	
	packetData = (PBYTE)ExAllocatePool(NonPagedPool, dataLength + 1);
	if(packetData == NULL) 
		{
			if(gDBG_Mode) DbgPrint("Not enough memory to allocate pool for packet data.\n");
			goto Exit;
		}
	FwpsCopyStreamDataToBuffer(streamData, packetData, dataLength, &bytesCopied);
	
	for(int i = 0; i < bytesCopied; i++)
		DbgPrint("%c", *(packetData + i));
	ExFreePool(packetData);
	

	//
	// In this sample we don't edit TCP urgent data
	//

	if ((streamData->flags & FWPS_STREAM_FLAG_SEND_EXPEDITED) ||
		(streamData->flags & FWPS_STREAM_FLAG_RECEIVE_EXPEDITED))
	{
		ioPacket->streamAction = FWPS_STREAM_ACTION_NONE;
		classifyOut->actionType = FWP_ACTION_PERMIT;
		goto Exit;
	}

	classifyOut->actionType = FWP_ACTION_PERMIT;
	ioPacket->streamAction = FWPS_STREAM_ACTION_NONE;

Exit:

	return;
}

// -------------------------------------------------------------------------------------
// Функция-нотификатор не используется в этом проекте. Дабы засуппресить Warning при W4
// используется макрос UNREFERENCED_PARAMETER, так как функция пустая по своей сути. 

NTSTATUS NTAPI TrafficBasicNotifyFn(
	IN FWPS_CALLOUT_NOTIFY_TYPE notifyType,
	IN const GUID* filterKey,
	IN const FWPS_FILTER* filter
	)
   {
	   UNREFERENCED_PARAMETER(notifyType);
	   UNREFERENCED_PARAMETER(filterKey);
	   UNREFERENCED_PARAMETER(filter);

	   return STATUS_SUCCESS;
   }

// --------------------- Автогенерированные снипеты для общих функций драйвера --------------

NTSTATUS basic_wfpCreateClose(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp)
{
	Irp->IoStatus.Status = STATUS_SUCCESS;
	Irp->IoStatus.Information = 0;
	IoCompleteRequest(Irp, IO_NO_INCREMENT);
	return STATUS_SUCCESS;
}

NTSTATUS basic_wfpDefaultHandler(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp)
{
	Pbasic_wfp_DEVICE_EXTENSION deviceExtension = NULL;

	IoSkipCurrentIrpStackLocation(Irp);
	deviceExtension = (Pbasic_wfp_DEVICE_EXTENSION) DeviceObject->DeviceExtension;
	return IoCallDriver(deviceExtension->TargetDeviceObject, Irp);
}

NTSTATUS basic_wfpAddDevice(IN PDRIVER_OBJECT  DriverObject, IN PDEVICE_OBJECT  PhysicalDeviceObject)
{
	PDEVICE_OBJECT DeviceObject = NULL;
	Pbasic_wfp_DEVICE_EXTENSION pExtension = NULL;
	NTSTATUS status;

	status = IoCreateDevice(DriverObject,
		sizeof(basic_wfp_DEVICE_EXTENSION),
		NULL,
		FILE_DEVICE_UNKNOWN,
		0,
		0,
		&DeviceObject);

	if (!NT_SUCCESS(status))
		return status;

	pExtension = (Pbasic_wfp_DEVICE_EXTENSION)DeviceObject->DeviceExtension;

	pExtension->DeviceObject = DeviceObject;
	pExtension->PhysicalDeviceObject = PhysicalDeviceObject;
	pExtension->TargetDeviceObject = IoAttachDeviceToDeviceStack(DeviceObject, PhysicalDeviceObject);

	status = IoRegisterDeviceInterface(PhysicalDeviceObject, &GUID_basic_wfpInterface, NULL, &pExtension->DeviceInterface);
	ASSERT(NT_SUCCESS(status));

	DeviceObject->Flags &= ~DO_DEVICE_INITIALIZING;
	return STATUS_SUCCESS;
}


NTSTATUS basic_wfpIrpCompletion(
	IN PDEVICE_OBJECT DeviceObject,
	IN PIRP Irp,
	IN PVOID Context
	)
{
	PKEVENT Event = (PKEVENT) Context;

	UNREFERENCED_PARAMETER(DeviceObject);
	UNREFERENCED_PARAMETER(Irp);

	KeSetEvent(Event, IO_NO_INCREMENT, FALSE);

	return(STATUS_MORE_PROCESSING_REQUIRED);
}

NTSTATUS basic_wfpForwardIrpSynchronous(
	IN PDEVICE_OBJECT DeviceObject,
	IN PIRP Irp
	)
{
	Pbasic_wfp_DEVICE_EXTENSION   deviceExtension;
	KEVENT event;
	NTSTATUS status;

	KeInitializeEvent(&event, NotificationEvent, FALSE);
	deviceExtension = (Pbasic_wfp_DEVICE_EXTENSION) DeviceObject->DeviceExtension;

	IoCopyCurrentIrpStackLocationToNext(Irp);

	IoSetCompletionRoutine(Irp, basic_wfpIrpCompletion, &event, TRUE, TRUE, TRUE);

	status = IoCallDriver(deviceExtension->TargetDeviceObject, Irp);

	if (status == STATUS_PENDING) {
		KeWaitForSingleObject(&event, Executive, KernelMode, FALSE, NULL);
		status = Irp->IoStatus.Status;
	}
	return status;
}

NTSTATUS basic_wfpPnP(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp)
{
	PIO_STACK_LOCATION irpSp = IoGetCurrentIrpStackLocation(Irp);
	Pbasic_wfp_DEVICE_EXTENSION pExt = ((Pbasic_wfp_DEVICE_EXTENSION)DeviceObject->DeviceExtension);
	NTSTATUS status;

	ASSERT(pExt);

	switch (irpSp->MinorFunction)
	{
	case IRP_MN_START_DEVICE:
		IoSetDeviceInterfaceState(&pExt->DeviceInterface, TRUE);
		Irp->IoStatus.Status = STATUS_SUCCESS;
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		return STATUS_SUCCESS;

	case IRP_MN_QUERY_REMOVE_DEVICE:
		Irp->IoStatus.Status = STATUS_SUCCESS;
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		return STATUS_SUCCESS;

	case IRP_MN_REMOVE_DEVICE:
		IoSetDeviceInterfaceState(&pExt->DeviceInterface, FALSE);
		status = basic_wfpForwardIrpSynchronous(DeviceObject, Irp);
		IoDetachDevice(pExt->TargetDeviceObject);
		IoDeleteDevice(pExt->DeviceObject);
		RtlFreeUnicodeString(&pExt->DeviceInterface);
		Irp->IoStatus.Status = STATUS_SUCCESS;
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		return STATUS_SUCCESS;

	case IRP_MN_QUERY_PNP_DEVICE_STATE:
		status = basic_wfpForwardIrpSynchronous(DeviceObject, Irp);
		Irp->IoStatus.Information = 0;
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		return status;
	}
	return basic_wfpDefaultHandler(DeviceObject, Irp);
}