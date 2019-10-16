/* Copyright (C) 2019 Interactive Brokers LLC. All rights reserved. This code is subject to the terms
 * and conditions of the IB API Non-Commercial License or the IB API Commercial License, as applicable. */

#pragma once
#ifndef TWS_API_SAMPLES_TESTCPPCLIENT_TESTCPPCLIENT_H
#define TWS_API_SAMPLES_TESTCPPCLIENT_TESTCPPCLIENT_H

#include "EWrapper.h"
#include "EReaderOSSignal.h"
#include "EReader.h"

#include <memory>
#include <vector>

class EClientSocket;

//! [ewrapperimpl]
class GaucheAdapter : public EWrapper
{
//! [ewrapperimpl]
public:

	GaucheAdapter();
	~GaucheAdapter();

	void setConnectOptions(const std::string&);
	void processMessages();

public:

	bool connect(const char * host, int port, int clientId = 0);
	void disconnect() const;
	bool isConnected() const;

public:
	void historicalDataRequests(TickerId reqId, const char* symbol,
                                const char* secType, const char* currency,
                                const char* exchange, const char* queryTime,
                                const char* duration, const char* barSize,
                                const char* whatToShow);
    void requestCurrentTime();

    void placeFxMarketOrder(OrderId orderId, const char* symbol,
                            const char* secType,
                            const char* currency, const char* exchange,
                            const char* action, double quantity);

public:
	// events
	#include "EWrapper_prototypes.h"


private:
	void printContractMsg(const Contract& contract);
	void printContractDetailsMsg(const ContractDetails& contractDetails);
	void printContractDetailsSecIdList(const TagValueListSPtr &secIdList);
	void printBondContractDetailsMsg(const ContractDetails& contractDetails);

private:
	//! [socket_declare]
	EReaderOSSignal m_osSignal;
	EClientSocket * const m_pClient;
	//! [socket_declare]
	// State m_state;
	time_t m_sleepDeadline;

	// OrderId m_orderId;
	EReader *m_pReader;
    bool m_extraAuth;
	std::string m_bboExchange;
};

#endif

// Local Variables:
// mode: c++
// tab-width: 4
// c-basic-offset: 4
// End:
