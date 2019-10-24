#include <gauche.h>
#include <gauche/extend.h>

#include "Contract.h"
#include "Order.h"

#include "GaucheAdapter.h"

SCM_DECL_BEGIN

extern ScmClass *TwsClientClass;

#define TWS_CLIENT_P(obj)     SCM_XTYPEP(obj, TwsClientClass)
#define TWS_CLIENT_UNBOX(obj) SCM_FOREIGN_POINTER_REF(GaucheAdapter*, obj)
#define TWS_CLIENT_BOX(ptr)   Scm_MakeForeignPointer(TwsClientClass, ptr)

extern void Scm_Init_tws_client();

//////
extern ScmClass *TwsContractClass;

#define TWS_CONTRACT_P(obj)     SCM_XTYPEP(obj, TwsContractClass)
#define TWS_CONTRACT_UNBOX(obj) SCM_FOREIGN_POINTER_REF(Contract*, obj)
#define TWS_CONTRACT_BOX(ptr)   Scm_MakeForeignPointer(TwsContractClass, ptr)

extern void Scm_Init_tws_contract();

//////
extern ScmClass *TwsOrderClass;

#define TWS_ORDER_P(obj)     SCM_XTYPEP(obj, TwsOrderClass)
#define TWS_ORDER_UNBOX(obj) SCM_FOREIGN_POINTER_REF(Order*, obj)
#define TWS_ORDER_BOX(ptr)   Scm_MakeForeignPointer(TwsOrderClass, ptr)

extern void Scm_Init_tws_order();

SCM_DECL_END
