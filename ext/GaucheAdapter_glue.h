#include <gauche.h>
#include <gauche/extend.h>

#include "GaucheAdapter.h"

SCM_DECL_BEGIN

extern ScmClass *TwsClientClass;

#define TWS_CLIENT_P(obj)     SCM_XTYPEP(obj, TwsClientClass)
#define TWS_CLIENT_UNBOX(obj) SCM_FOREIGN_POINTER_REF(GaucheAdapter*, obj)
#define TWS_CLIENT_BOX(ptr)   Scm_MakeForeignPointer(TwsClientClass, ptr)

extern void Scm_Init_tws_client();

SCM_DECL_END
