/* Copyright (C) 2019 Interactive Brokers LLC. All rights reserved. This code is subject to the terms
 * and conditions of the IB API Non-Commercial License or the IB API Commercial License, as applicable. */

#include "StdAfx.h"

#include "GaucheAdapter.h"

#include "EClientSocket.h"
#include "EPosixClientSocketPlatform.h"

#include "Contract.h"
#include "Order.h"
#include "OrderState.h"
#include "Execution.h"
#include "CommissionReport.h"
#include "Utils.h"

#include <stdio.h>
#include <chrono>
#include <iostream>
#include <thread>
#include <ctime>
#include <fstream>
#include <cstdint>

#include <gauche.h>
#include <gauche/static.h>

///////////////////////////////////////////////////////////
// member funcs
//! [socket_init]
GaucheAdapter::GaucheAdapter() :
      m_osSignal(2000)//2-seconds timeout
    , m_pClient(new EClientSocket(this, &m_osSignal))
	, m_sleepDeadline(0)
	// , m_orderId(0)
    , m_pReader(0)
    , m_extraAuth(false)
{
}
//! [socket_init]
GaucheAdapter::~GaucheAdapter()
{
    if (m_pReader)
        delete m_pReader;

    delete m_pClient;
}

bool GaucheAdapter::connect(const char *host, int port, int clientId)
{
	// trying to connect
	Scm_Printf(SCM_CURERR, "Connecting to %s:%d clientId:%d\n", !( host && *host) ? "127.0.0.1" : host, port, clientId);
	
	//! [connect]
	bool bRes = m_pClient->eConnect( host, port, clientId, m_extraAuth);
	//! [connect]
	
	if (bRes) {
		Scm_Printf(SCM_CURERR, "Connected to %s:%d clientId:%d\n", m_pClient->host().c_str(), m_pClient->port(), clientId);
		//! [ereader]
        m_pReader = new EReader(m_pClient, &m_osSignal);
		m_pReader->start();
		//! [ereader]
	}
	else
		Scm_Printf(SCM_CURERR, "Cannot connect to %s:%d clientId:%d\n", m_pClient->host().c_str(), m_pClient->port(), clientId);

	return bRes;
}

void GaucheAdapter::disconnect() const
{
	m_pClient->eDisconnect();

	Scm_Printf(SCM_CURERR, "Disconnected\n");
}

bool GaucheAdapter::isConnected() const
{
	return m_pClient->isConnected();
}

void GaucheAdapter::setConnectOptions(const std::string& connectOptions)
{
	m_pClient->setConnectOptions(connectOptions);
}

void GaucheAdapter::processMessages()
{
	m_osSignal.waitForSignal();
	errno = 0;
	m_pReader->processMsgs();
}

//////////////////////////////////////////////////////////////////
// methods
//! [connectack]
void GaucheAdapter::connectAck() {
	if (!m_extraAuth && m_pClient->asyncEConnect())
        m_pClient->startApi();
}
//! [connectack]

void GaucheAdapter::historicalDataRequests(TickerId reqId, const char* symbol,
                                           const char* secType, const char* currency,
                                           const char* exchange, const char* queryTime,
                                           const char* duration, const char* barSize,
                                           const char* whatToShow) {
	Contract contract;
	contract.symbol = symbol;
	contract.secType = secType;
	contract.currency = currency;
	contract.exchange = exchange;

    Scm_Printf(SCM_CURERR, "Historical Data requested\n");

	m_pClient->reqHistoricalData(reqId, contract,
                                 queryTime, duration, barSize, whatToShow, 1, 1, false,
                                 TagValueListSPtr());
}

void GaucheAdapter::placeFxMarketOrder(OrderId orderId, const char* symbol,
                                       const char* currency, const char* exchange,
                                       const char* action, double quantity) {
	Contract contract;
	contract.symbol = symbol;
	contract.secType = "CASH";
	contract.currency = currency;
	contract.exchange = exchange;

	Order order;
	order.action = action;
	order.orderType = "MKT";
	order.totalQuantity = quantity;

    m_pClient->placeOrder(orderId, contract, order);
}

void GaucheAdapter::requestCurrentTime() {
  m_pClient->reqCurrentTime();
}

static void scm_error(ScmObj c) {
    ScmObj m = Scm_ConditionMessage(c);
    if (SCM_FALSEP(m)) {
        Scm_Printf(SCM_CURERR, "gosh: Thrown unknown condition: %S\n", c);
    } else {
        Scm_ReportError(c, SCM_TRUE);
    }
}

//! [nextvalidid]
void GaucheAdapter::nextValidId( OrderId orderId)
{
	Scm_Printf(SCM_CURERR, "Next Valid Id: %ld\n", orderId);
	// m_orderId = orderId;
	//! [nextvalidid]

    ScmObj proc = SCM_UNDEFINED;
    SCM_BIND_PROC(proc, "on-next-valid-id", Scm_CurrentModule());
    ScmEvalPacket epak;
    ScmObj arglist = SCM_LIST1(Scm_MakeInteger(orderId));
    if (Scm_Apply(proc, arglist, &epak) < 0) {
        scm_error(epak.exception);
    }
}


void GaucheAdapter::currentTime(long time) {
	// Scm_Printf(SCM_CURERR, "Current Time: %ld\n", time);

    ScmObj proc = SCM_UNDEFINED;
    SCM_BIND_PROC(proc, "on-current-time", Scm_CurrentModule());
    ScmEvalPacket epak;
    ScmObj arglist = SCM_LIST1(Scm_MakeInteger(time));
    if (Scm_Apply(proc, arglist, &epak) < 0) {
        scm_error(epak.exception);
    }
}

//! [error]
void GaucheAdapter::error(int id, int errorCode, const std::string& errorString)
{
	Scm_Printf(SCM_CURERR, "Error. Id: %d, Code: %d, Msg: %s\n", id, errorCode, errorString.c_str());
}
//! [error]

//! [tickprice]
void GaucheAdapter::tickPrice( TickerId tickerId, TickType field, double price, const TickAttrib& attribs) {
	Scm_Printf(SCM_CURERR, "Tick Price. Ticker Id: %ld, Field: %d, Price: %g, CanAutoExecute: %d, PastLimit: %d, PreOpen: %d\n", tickerId, (int)field, price, attribs.canAutoExecute, attribs.pastLimit, attribs.preOpen);
}
//! [tickprice]

//! [ticksize]
void GaucheAdapter::tickSize( TickerId tickerId, TickType field, int size) {
	Scm_Printf(SCM_CURERR, "Tick Size. Ticker Id: %ld, Field: %d, Size: %d\n", tickerId, (int)field, size);
}
//! [ticksize]

//! [tickoptioncomputation]
void GaucheAdapter::tickOptionComputation( TickerId tickerId, TickType tickType, double impliedVol, double delta,
                                          double optPrice, double pvDividend,
                                          double gamma, double vega, double theta, double undPrice) {
	Scm_Printf(SCM_CURERR, "TickOptionComputation. Ticker Id: %ld, Type: %d, ImpliedVolatility: %g, Delta: %g, OptionPrice: %g, pvDividend: %g, Gamma: %g, Vega: %g, Theta: %g, Underlying Price: %g\n", tickerId, (int)tickType, impliedVol, delta, optPrice, pvDividend, gamma, vega, theta, undPrice);
}
//! [tickoptioncomputation]

//! [tickgeneric]
void GaucheAdapter::tickGeneric(TickerId tickerId, TickType tickType, double value) {
	Scm_Printf(SCM_CURERR, "Tick Generic. Ticker Id: %ld, Type: %d, Value: %g\n", tickerId, (int)tickType, value);
}
//! [tickgeneric]

//! [tickstring]
void GaucheAdapter::tickString(TickerId tickerId, TickType tickType, const std::string& value) {
	Scm_Printf(SCM_CURERR, "Tick String. Ticker Id: %ld, Type: %d, Value: %s\n", tickerId, (int)tickType, value.c_str());
}
//! [tickstring]

void GaucheAdapter::tickEFP(TickerId tickerId, TickType tickType, double basisPoints, const std::string& formattedBasisPoints,
                            double totalDividends, int holdDays, const std::string& futureLastTradeDate, double dividendImpact, double dividendsToLastTradeDate) {
	Scm_Printf(SCM_CURERR, "TickEFP. %ld, Type: %d, BasisPoints: %g, FormattedBasisPoints: %s, Total Dividends: %g, HoldDays: %d, Future Last Trade Date: %s, Dividend Impact: %g, Dividends To Last Trade Date: %g\n", tickerId, (int)tickType, basisPoints, formattedBasisPoints.c_str(), totalDividends, holdDays, futureLastTradeDate.c_str(), dividendImpact, dividendsToLastTradeDate);
}

//! [orderstatus]
void GaucheAdapter::orderStatus(OrderId orderId, const std::string& status, double filled,
		double remaining, double avgFillPrice, int permId, int parentId,
		double lastFillPrice, int clientId, const std::string& whyHeld, double mktCapPrice){
	Scm_Printf(SCM_CURERR, "OrderStatus. Id: %ld, Status: %s, Filled: %g, Remaining: %g, AvgFillPrice: %g, PermId: %d, LastFillPrice: %g, ClientId: %d, WhyHeld: %s, MktCapPrice: %g\n", orderId, status.c_str(), filled, remaining, avgFillPrice, permId, lastFillPrice, clientId, whyHeld.c_str(), mktCapPrice);
}
//! [orderstatus]

//! [openorder]
void GaucheAdapter::openOrder( OrderId orderId, const Contract& contract, const Order& order, const OrderState& orderState) {
	Scm_Printf(SCM_CURERR, "OpenOrder. PermId: %ld, ClientId: %ld, OrderId: %ld, Account: %s, Symbol: %s, SecType: %s, Exchange: %s:, Action: %s, OrderType:%s, TotalQty: %g, CashQty: %g, "
	"LmtPrice: %g, AuxPrice: %g, Status: %s\n", 
		order.permId, order.clientId, orderId, order.account.c_str(), contract.symbol.c_str(), contract.secType.c_str(), contract.exchange.c_str(), 
		order.action.c_str(), order.orderType.c_str(), order.totalQuantity, order.cashQty == UNSET_DOUBLE ? 0 : order.cashQty, order.lmtPrice, order.auxPrice, orderState.status.c_str());
}
//! [openorder]

//! [openorderend]
void GaucheAdapter::openOrderEnd() {
	Scm_Printf(SCM_CURERR, "OpenOrderEnd\n");
}
//! [openorderend]

void GaucheAdapter::winError( const std::string& str, int lastError) {}
void GaucheAdapter::connectionClosed() {
  printf("Connection Closed\n");
	// Scm_Printf(SCM_CURERR, "Connection Closed\n");
}

//! [updateaccountvalue]
void GaucheAdapter::updateAccountValue(const std::string& key, const std::string& val,
                                       const std::string& currency, const std::string& accountName) {
	Scm_Printf(SCM_CURERR, "UpdateAccountValue. Key: %s, Value: %s, Currency: %s, Account Name: %s\n", key.c_str(), val.c_str(), currency.c_str(), accountName.c_str());
}
//! [updateaccountvalue]

//! [updateportfolio]
void GaucheAdapter::updatePortfolio(const Contract& contract, double position,
                                    double marketPrice, double marketValue, double averageCost,
                                    double unrealizedPNL, double realizedPNL, const std::string& accountName){
	Scm_Printf(SCM_CURERR, "UpdatePortfolio. %s, %s @ %s: Position: %g, MarketPrice: %g, MarketValue: %g, AverageCost: %g, UnrealizedPNL: %g, RealizedPNL: %g, AccountName: %s\n", (contract.symbol).c_str(), (contract.secType).c_str(), (contract.primaryExchange).c_str(), position, marketPrice, marketValue, averageCost, unrealizedPNL, realizedPNL, accountName.c_str());
}
//! [updateportfolio]

//! [updateaccounttime]
void GaucheAdapter::updateAccountTime(const std::string& timeStamp) {
	Scm_Printf(SCM_CURERR, "UpdateAccountTime. Time: %s\n", timeStamp.c_str());
}
//! [updateaccounttime]

//! [accountdownloadend]
void GaucheAdapter::accountDownloadEnd(const std::string& accountName) {
	Scm_Printf(SCM_CURERR, "Account download finished: %s\n", accountName.c_str());
}
//! [accountdownloadend]

//! [contractdetails]
void GaucheAdapter::contractDetails( int reqId, const ContractDetails& contractDetails) {
	Scm_Printf(SCM_CURERR, "ContractDetails begin. ReqId: %d\n", reqId);
	printContractMsg(contractDetails.contract);
	printContractDetailsMsg(contractDetails);
	Scm_Printf(SCM_CURERR, "ContractDetails end. ReqId: %d\n", reqId);
}
//! [contractdetails]

//! [bondcontractdetails]
void GaucheAdapter::bondContractDetails( int reqId, const ContractDetails& contractDetails) {
	Scm_Printf(SCM_CURERR, "BondContractDetails begin. ReqId: %d\n", reqId);
	printBondContractDetailsMsg(contractDetails);
	Scm_Printf(SCM_CURERR, "BondContractDetails end. ReqId: %d\n", reqId);
}
//! [bondcontractdetails]

void GaucheAdapter::printContractMsg(const Contract& contract) {
	Scm_Printf(SCM_CURERR, "\tConId: %ld\n", contract.conId);
	Scm_Printf(SCM_CURERR, "\tSymbol: %s\n", contract.symbol.c_str());
	Scm_Printf(SCM_CURERR, "\tSecType: %s\n", contract.secType.c_str());
	Scm_Printf(SCM_CURERR, "\tLastTradeDateOrContractMonth: %s\n", contract.lastTradeDateOrContractMonth.c_str());
	Scm_Printf(SCM_CURERR, "\tStrike: %g\n", contract.strike);
	Scm_Printf(SCM_CURERR, "\tRight: %s\n", contract.right.c_str());
	Scm_Printf(SCM_CURERR, "\tMultiplier: %s\n", contract.multiplier.c_str());
	Scm_Printf(SCM_CURERR, "\tExchange: %s\n", contract.exchange.c_str());
	Scm_Printf(SCM_CURERR, "\tPrimaryExchange: %s\n", contract.primaryExchange.c_str());
	Scm_Printf(SCM_CURERR, "\tCurrency: %s\n", contract.currency.c_str());
	Scm_Printf(SCM_CURERR, "\tLocalSymbol: %s\n", contract.localSymbol.c_str());
	Scm_Printf(SCM_CURERR, "\tTradingClass: %s\n", contract.tradingClass.c_str());
}

void GaucheAdapter::printContractDetailsMsg(const ContractDetails& contractDetails) {
	Scm_Printf(SCM_CURERR, "\tMarketName: %s\n", contractDetails.marketName.c_str());
	Scm_Printf(SCM_CURERR, "\tMinTick: %g\n", contractDetails.minTick);
	Scm_Printf(SCM_CURERR, "\tPriceMagnifier: %ld\n", contractDetails.priceMagnifier);
	Scm_Printf(SCM_CURERR, "\tOrderTypes: %s\n", contractDetails.orderTypes.c_str());
	Scm_Printf(SCM_CURERR, "\tValidExchanges: %s\n", contractDetails.validExchanges.c_str());
	Scm_Printf(SCM_CURERR, "\tUnderConId: %d\n", contractDetails.underConId);
	Scm_Printf(SCM_CURERR, "\tLongName: %s\n", contractDetails.longName.c_str());
	Scm_Printf(SCM_CURERR, "\tContractMonth: %s\n", contractDetails.contractMonth.c_str());
	Scm_Printf(SCM_CURERR, "\tIndystry: %s\n", contractDetails.industry.c_str());
	Scm_Printf(SCM_CURERR, "\tCategory: %s\n", contractDetails.category.c_str());
	Scm_Printf(SCM_CURERR, "\tSubCategory: %s\n", contractDetails.subcategory.c_str());
	Scm_Printf(SCM_CURERR, "\tTimeZoneId: %s\n", contractDetails.timeZoneId.c_str());
	Scm_Printf(SCM_CURERR, "\tTradingHours: %s\n", contractDetails.tradingHours.c_str());
	Scm_Printf(SCM_CURERR, "\tLiquidHours: %s\n", contractDetails.liquidHours.c_str());
	Scm_Printf(SCM_CURERR, "\tEvRule: %s\n", contractDetails.evRule.c_str());
	Scm_Printf(SCM_CURERR, "\tEvMultiplier: %g\n", contractDetails.evMultiplier);
	Scm_Printf(SCM_CURERR, "\tMdSizeMultiplier: %d\n", contractDetails.mdSizeMultiplier);
	Scm_Printf(SCM_CURERR, "\tAggGroup: %d\n", contractDetails.aggGroup);
	Scm_Printf(SCM_CURERR, "\tUnderSymbol: %s\n", contractDetails.underSymbol.c_str());
	Scm_Printf(SCM_CURERR, "\tUnderSecType: %s\n", contractDetails.underSecType.c_str());
	Scm_Printf(SCM_CURERR, "\tMarketRuleIds: %s\n", contractDetails.marketRuleIds.c_str());
	Scm_Printf(SCM_CURERR, "\tRealExpirationDate: %s\n", contractDetails.realExpirationDate.c_str());
	Scm_Printf(SCM_CURERR, "\tLastTradeTime: %s\n", contractDetails.lastTradeTime.c_str());
	printContractDetailsSecIdList(contractDetails.secIdList);
}

void GaucheAdapter::printContractDetailsSecIdList(const TagValueListSPtr &secIdList) {
	const int secIdListCount = secIdList.get() ? secIdList->size() : 0;
	if (secIdListCount > 0) {
		Scm_Printf(SCM_CURERR, "\tSecIdList: {");
		for (int i = 0; i < secIdListCount; ++i) {
			const TagValue* tagValue = ((*secIdList)[i]).get();
			Scm_Printf(SCM_CURERR, "%s=%s;",tagValue->tag.c_str(), tagValue->value.c_str());
		}
		Scm_Printf(SCM_CURERR, "}\n");
	}
}

void GaucheAdapter::printBondContractDetailsMsg(const ContractDetails& contractDetails) {
	Scm_Printf(SCM_CURERR, "\tSymbol: %s\n", contractDetails.contract.symbol.c_str());
	Scm_Printf(SCM_CURERR, "\tSecType: %s\n", contractDetails.contract.secType.c_str());
	Scm_Printf(SCM_CURERR, "\tCusip: %s\n", contractDetails.cusip.c_str());
	Scm_Printf(SCM_CURERR, "\tCoupon: %g\n", contractDetails.coupon);
	Scm_Printf(SCM_CURERR, "\tMaturity: %s\n", contractDetails.maturity.c_str());
	Scm_Printf(SCM_CURERR, "\tIssueDate: %s\n", contractDetails.issueDate.c_str());
	Scm_Printf(SCM_CURERR, "\tRatings: %s\n", contractDetails.ratings.c_str());
	Scm_Printf(SCM_CURERR, "\tBondType: %s\n", contractDetails.bondType.c_str());
	Scm_Printf(SCM_CURERR, "\tCouponType: %s\n", contractDetails.couponType.c_str());
	Scm_Printf(SCM_CURERR, "\tConvertible: %s\n", contractDetails.convertible ? "yes" : "no");
	Scm_Printf(SCM_CURERR, "\tCallable: %s\n", contractDetails.callable ? "yes" : "no");
	Scm_Printf(SCM_CURERR, "\tPutable: %s\n", contractDetails.putable ? "yes" : "no");
	Scm_Printf(SCM_CURERR, "\tDescAppend: %s\n", contractDetails.descAppend.c_str());
	Scm_Printf(SCM_CURERR, "\tExchange: %s\n", contractDetails.contract.exchange.c_str());
	Scm_Printf(SCM_CURERR, "\tCurrency: %s\n", contractDetails.contract.currency.c_str());
	Scm_Printf(SCM_CURERR, "\tMarketName: %s\n", contractDetails.marketName.c_str());
	Scm_Printf(SCM_CURERR, "\tTradingClass: %s\n", contractDetails.contract.tradingClass.c_str());
	Scm_Printf(SCM_CURERR, "\tConId: %ld\n", contractDetails.contract.conId);
	Scm_Printf(SCM_CURERR, "\tMinTick: %g\n", contractDetails.minTick);
	Scm_Printf(SCM_CURERR, "\tMdSizeMultiplier: %d\n", contractDetails.mdSizeMultiplier);
	Scm_Printf(SCM_CURERR, "\tOrderTypes: %s\n", contractDetails.orderTypes.c_str());
	Scm_Printf(SCM_CURERR, "\tValidExchanges: %s\n", contractDetails.validExchanges.c_str());
	Scm_Printf(SCM_CURERR, "\tNextOptionDate: %s\n", contractDetails.nextOptionDate.c_str());
	Scm_Printf(SCM_CURERR, "\tNextOptionType: %s\n", contractDetails.nextOptionType.c_str());
	Scm_Printf(SCM_CURERR, "\tNextOptionPartial: %s\n", contractDetails.nextOptionPartial ? "yes" : "no");
	Scm_Printf(SCM_CURERR, "\tNotes: %s\n", contractDetails.notes.c_str());
	Scm_Printf(SCM_CURERR, "\tLong Name: %s\n", contractDetails.longName.c_str());
	Scm_Printf(SCM_CURERR, "\tEvRule: %s\n", contractDetails.evRule.c_str());
	Scm_Printf(SCM_CURERR, "\tEvMultiplier: %g\n", contractDetails.evMultiplier);
	Scm_Printf(SCM_CURERR, "\tAggGroup: %d\n", contractDetails.aggGroup);
	Scm_Printf(SCM_CURERR, "\tMarketRuleIds: %s\n", contractDetails.marketRuleIds.c_str());
	Scm_Printf(SCM_CURERR, "\tTimeZoneId: %s\n", contractDetails.timeZoneId.c_str());
	Scm_Printf(SCM_CURERR, "\tLastTradeTime: %s\n", contractDetails.lastTradeTime.c_str());
	printContractDetailsSecIdList(contractDetails.secIdList);
}

//! [contractdetailsend]
void GaucheAdapter::contractDetailsEnd( int reqId) {
	Scm_Printf(SCM_CURERR, "ContractDetailsEnd. %d\n", reqId);
}
//! [contractdetailsend]

//! [execdetails]
void GaucheAdapter::execDetails( int reqId, const Contract& contract, const Execution& execution) {
	Scm_Printf(SCM_CURERR, "ExecDetails. ReqId: %d - %s, %s, %s - %s, %ld, %g, %d\n", reqId, contract.symbol.c_str(), contract.secType.c_str(), contract.currency.c_str(), execution.execId.c_str(), execution.orderId, execution.shares, execution.lastLiquidity);
}
//! [execdetails]

//! [execdetailsend]
void GaucheAdapter::execDetailsEnd( int reqId) {
	Scm_Printf(SCM_CURERR, "ExecDetailsEnd. %d\n", reqId);
}
//! [execdetailsend]

//! [updatemktdepth]
void GaucheAdapter::updateMktDepth(TickerId id, int position, int operation, int side,
                                   double price, int size) {
	Scm_Printf(SCM_CURERR, "UpdateMarketDepth. %ld - Position: %d, Operation: %d, Side: %d, Price: %g, Size: %d\n", id, position, operation, side, price, size);
}
//! [updatemktdepth]

//! [updatemktdepthl2]
void GaucheAdapter::updateMktDepthL2(TickerId id, int position, const std::string& marketMaker, int operation,
                                     int side, double price, int size, bool isSmartDepth) {
	Scm_Printf(SCM_CURERR, "UpdateMarketDepthL2. %ld - Position: %d, Operation: %d, Side: %d, Price: %g, Size: %d, isSmartDepth: %d\n", id, position, operation, side, price, size, isSmartDepth);
}
//! [updatemktdepthl2]

//! [updatenewsbulletin]
void GaucheAdapter::updateNewsBulletin(int msgId, int msgType, const std::string& newsMessage, const std::string& originExch) {
	Scm_Printf(SCM_CURERR, "News Bulletins. %d - Type: %d, Message: %s, Exchange of Origin: %s\n", msgId, msgType, newsMessage.c_str(), originExch.c_str());
}
//! [updatenewsbulletin]

//! [managedaccounts]
void GaucheAdapter::managedAccounts( const std::string& accountsList) {
	Scm_Printf(SCM_CURERR, "Account List: %s\n", accountsList.c_str());
}
//! [managedaccounts]

//! [receivefa]
void GaucheAdapter::receiveFA(faDataType pFaDataType, const std::string& cxml) {
	std::cout << "Receiving FA: " << (int)pFaDataType << std::endl << cxml << std::endl;
}
//! [receivefa]

//! [historicaldata]
void GaucheAdapter::historicalData(TickerId reqId, const Bar& bar) {
	Scm_Printf(SCM_CURERR, "HistoricalData. ReqId: %ld - Date: %s, Open: %g, High: %g, Low: %g, Close: %g, Volume: %lld, Count: %d, WAP: %g\n", reqId, bar.time.c_str(), bar.open, bar.high, bar.low, bar.close, bar.volume, bar.count, bar.wap);

    ScmObj proc = SCM_UNDEFINED;
    SCM_BIND_PROC(proc, "on-historical-data", Scm_CurrentModule());
    ScmEvalPacket epak;
    ScmObj arglist =
        Scm_Cons(Scm_MakeInteger(reqId),
                 Scm_Cons(SCM_MAKE_STR(bar.time.c_str()),
                          Scm_Cons(Scm_MakeFlonum(bar.open),
                                   Scm_Cons(Scm_MakeFlonum(bar.high),
                                            SCM_LIST5(Scm_MakeFlonum(bar.low),
                                                      Scm_MakeFlonum(bar.close),
                                                      Scm_MakeInteger(bar.volume),
                                                      Scm_MakeInteger(bar.count),
                                                      Scm_MakeFlonum(bar.wap)
                                                      )))));
    if (Scm_Apply(proc, arglist, &epak) < 0) {
        scm_error(epak.exception);
    }
}
//! [historicaldata]

//! [historicaldataend]
void GaucheAdapter::historicalDataEnd(int reqId, const std::string& startDateStr, const std::string& endDateStr) {
	std::cout << "HistoricalDataEnd. ReqId: " << reqId << " - Start Date: " << startDateStr << ", End Date: " << endDateStr << std::endl;

    ScmObj proc = SCM_UNDEFINED;
    SCM_BIND_PROC(proc, "on-historical-data-end", Scm_CurrentModule());
    ScmEvalPacket epak;
    ScmObj arglist =
        SCM_LIST3(Scm_MakeInteger(reqId),
                  SCM_MAKE_STR(startDateStr.c_str()),
                  SCM_MAKE_STR(endDateStr.c_str()));

    if (Scm_Apply(proc, arglist, &epak) < 0) {
        scm_error(epak.exception);
    }
}
//! [historicaldataend]

//! [scannerparameters]
void GaucheAdapter::scannerParameters(const std::string& xml) {
	Scm_Printf(SCM_CURERR, "ScannerParameters. %s\n", xml.c_str());
}
//! [scannerparameters]

//! [scannerdata]
void GaucheAdapter::scannerData(int reqId, int rank, const ContractDetails& contractDetails,
                                const std::string& distance, const std::string& benchmark, const std::string& projection,
                                const std::string& legsStr) {
	Scm_Printf(SCM_CURERR, "ScannerData. %d - Rank: %d, Symbol: %s, SecType: %s, Currency: %s, Distance: %s, Benchmark: %s, Projection: %s, Legs String: %s\n", reqId, rank, contractDetails.contract.symbol.c_str(), contractDetails.contract.secType.c_str(), contractDetails.contract.currency.c_str(), distance.c_str(), benchmark.c_str(), projection.c_str(), legsStr.c_str());
}
//! [scannerdata]

//! [scannerdataend]
void GaucheAdapter::scannerDataEnd(int reqId) {
	Scm_Printf(SCM_CURERR, "ScannerDataEnd. %d\n", reqId);
}
//! [scannerdataend]

//! [realtimebar]
void GaucheAdapter::realtimeBar(TickerId reqId, long time, double open, double high, double low, double close,
                                long volume, double wap, int count) {
	Scm_Printf(SCM_CURERR, "RealTimeBars. %ld - Time: %ld, Open: %g, High: %g, Low: %g, Close: %g, Volume: %ld, Count: %d, WAP: %g\n", reqId, time, open, high, low, close, volume, count, wap);
}
//! [realtimebar]

//! [fundamentaldata]
void GaucheAdapter::fundamentalData(TickerId reqId, const std::string& data) {
	Scm_Printf(SCM_CURERR, "FundamentalData. ReqId: %ld, %s\n", reqId, data.c_str());
}
//! [fundamentaldata]

void GaucheAdapter::deltaNeutralValidation(int reqId, const DeltaNeutralContract& deltaNeutralContract) {
	Scm_Printf(SCM_CURERR, "DeltaNeutralValidation. %d, ConId: %ld, Delta: %g, Price: %g\n", reqId, deltaNeutralContract.conId, deltaNeutralContract.delta, deltaNeutralContract.price);
}

//! [ticksnapshotend]
void GaucheAdapter::tickSnapshotEnd(int reqId) {
	Scm_Printf(SCM_CURERR, "TickSnapshotEnd: %d\n", reqId);
}
//! [ticksnapshotend]

//! [marketdatatype]
void GaucheAdapter::marketDataType(TickerId reqId, int marketDataType) {
	Scm_Printf(SCM_CURERR, "MarketDataType. ReqId: %ld, Type: %d\n", reqId, marketDataType);
}
//! [marketdatatype]

//! [commissionreport]
void GaucheAdapter::commissionReport( const CommissionReport& commissionReport) {
	Scm_Printf(SCM_CURERR, "CommissionReport. %s - %g %s RPNL %g\n", commissionReport.execId.c_str(), commissionReport.commission, commissionReport.currency.c_str(), commissionReport.realizedPNL);
}
//! [commissionreport]

//! [position]
void GaucheAdapter::position( const std::string& account, const Contract& contract, double position, double avgCost) {
	Scm_Printf(SCM_CURERR, "Position. %s - Symbol: %s, SecType: %s, Currency: %s, Position: %g, Avg Cost: %g\n", account.c_str(), contract.symbol.c_str(), contract.secType.c_str(), contract.currency.c_str(), position, avgCost);
}
//! [position]

//! [positionend]
void GaucheAdapter::positionEnd() {
	Scm_Printf(SCM_CURERR, "PositionEnd\n");
}
//! [positionend]

//! [accountsummary]
void GaucheAdapter::accountSummary( int reqId, const std::string& account, const std::string& tag, const std::string& value, const std::string& currency) {
	Scm_Printf(SCM_CURERR, "Acct Summary. ReqId: %d, Account: %s, Tag: %s, Value: %s, Currency: %s\n", reqId, account.c_str(), tag.c_str(), value.c_str(), currency.c_str());
}
//! [accountsummary]

//! [accountsummaryend]
void GaucheAdapter::accountSummaryEnd( int reqId) {
	Scm_Printf(SCM_CURERR, "AccountSummaryEnd. Req Id: %d\n", reqId);
}
//! [accountsummaryend]

void GaucheAdapter::verifyMessageAPI( const std::string& apiData) {
	Scm_Printf(SCM_CURERR, "verifyMessageAPI: %s\b", apiData.c_str());
}

void GaucheAdapter::verifyCompleted( bool isSuccessful, const std::string& errorText) {
	Scm_Printf(SCM_CURERR, "verifyCompleted. IsSuccessfule: %d - Error: %s\n", isSuccessful, errorText.c_str());
}

void GaucheAdapter::verifyAndAuthMessageAPI( const std::string& apiDatai, const std::string& xyzChallenge) {
	Scm_Printf(SCM_CURERR, "verifyAndAuthMessageAPI: %s %s\n", apiDatai.c_str(), xyzChallenge.c_str());
}

void GaucheAdapter::verifyAndAuthCompleted( bool isSuccessful, const std::string& errorText) {
	Scm_Printf(SCM_CURERR, "verifyAndAuthCompleted. IsSuccessful: %d - Error: %s\n", isSuccessful, errorText.c_str());
    if (isSuccessful)
        m_pClient->startApi();
}

//! [displaygrouplist]
void GaucheAdapter::displayGroupList( int reqId, const std::string& groups) {
	Scm_Printf(SCM_CURERR, "Display Group List. ReqId: %d, Groups: %s\n", reqId, groups.c_str());
}
//! [displaygrouplist]

//! [displaygroupupdated]
void GaucheAdapter::displayGroupUpdated( int reqId, const std::string& contractInfo) {
	std::cout << "Display Group Updated. ReqId: " << reqId << ", Contract Info: " << contractInfo << std::endl;
}
//! [displaygroupupdated]

//! [positionmulti]
void GaucheAdapter::positionMulti( int reqId, const std::string& account,const std::string& modelCode, const Contract& contract, double pos, double avgCost) {
	Scm_Printf(SCM_CURERR, "Position Multi. Request: %d, Account: %s, ModelCode: %s, Symbol: %s, SecType: %s, Currency: %s, Position: %g, Avg Cost: %g\n", reqId, account.c_str(), modelCode.c_str(), contract.symbol.c_str(), contract.secType.c_str(), contract.currency.c_str(), pos, avgCost);
}
//! [positionmulti]

//! [positionmultiend]
void GaucheAdapter::positionMultiEnd( int reqId) {
	Scm_Printf(SCM_CURERR, "Position Multi End. Request: %d\n", reqId);
}
//! [positionmultiend]

//! [accountupdatemulti]
void GaucheAdapter::accountUpdateMulti( int reqId, const std::string& account, const std::string& modelCode, const std::string& key, const std::string& value, const std::string& currency) {
	Scm_Printf(SCM_CURERR, "AccountUpdate Multi. Request: %d, Account: %s, ModelCode: %s, Key, %s, Value: %s, Currency: %s\n", reqId, account.c_str(), modelCode.c_str(), key.c_str(), value.c_str(), currency.c_str());
}
//! [accountupdatemulti]

//! [accountupdatemultiend]
void GaucheAdapter::accountUpdateMultiEnd( int reqId) {
	Scm_Printf(SCM_CURERR, "Account Update Multi End. Request: %d\n", reqId);
}
//! [accountupdatemultiend]

//! [securityDefinitionOptionParameter]
void GaucheAdapter::securityDefinitionOptionalParameter(int reqId, const std::string& exchange, int underlyingConId, const std::string& tradingClass,
                                                        const std::string& multiplier, const std::set<std::string>& expirations, const std::set<double>& strikes) {
	Scm_Printf(SCM_CURERR, "Security Definition Optional Parameter. Request: %d, Trading Class: %s, Multiplier: %s\n", reqId, tradingClass.c_str(), multiplier.c_str());
}
//! [securityDefinitionOptionParameter]

//! [securityDefinitionOptionParameterEnd]
void GaucheAdapter::securityDefinitionOptionalParameterEnd(int reqId) {
	Scm_Printf(SCM_CURERR, "Security Definition Optional Parameter End. Request: %d\n", reqId);
}
//! [securityDefinitionOptionParameterEnd]

//! [softDollarTiers]
void GaucheAdapter::softDollarTiers(int reqId, const std::vector<SoftDollarTier> &tiers) {
	Scm_Printf(SCM_CURERR, "Soft dollar tiers (%lu):", tiers.size());

	for (unsigned int i = 0; i < tiers.size(); i++) {
		Scm_Printf(SCM_CURERR, "%s\n", tiers[i].displayName().c_str());
	}
}
//! [softDollarTiers]

//! [familyCodes]
void GaucheAdapter::familyCodes(const std::vector<FamilyCode> &familyCodes) {
	Scm_Printf(SCM_CURERR, "Family codes (%lu):\n", familyCodes.size());

	for (unsigned int i = 0; i < familyCodes.size(); i++) {
		Scm_Printf(SCM_CURERR, "Family code [%d] - accountID: %s familyCodeStr: %s\n", i, familyCodes[i].accountID.c_str(), familyCodes[i].familyCodeStr.c_str());
	}
}
//! [familyCodes]

//! [symbolSamples]
void GaucheAdapter::symbolSamples(int reqId, const std::vector<ContractDescription> &contractDescriptions) {
	Scm_Printf(SCM_CURERR, "Symbol Samples (total=%lu) reqId: %d\n", contractDescriptions.size(), reqId);

	for (unsigned int i = 0; i < contractDescriptions.size(); i++) {
		Contract contract = contractDescriptions[i].contract;
		std::vector<std::string> derivativeSecTypes = contractDescriptions[i].derivativeSecTypes;
		Scm_Printf(SCM_CURERR, "Contract (%u): %ld %s %s %s %s, ", i, contract.conId, contract.symbol.c_str(), contract.secType.c_str(), contract.primaryExchange.c_str(), contract.currency.c_str());
		Scm_Printf(SCM_CURERR, "Derivative Sec-types (%lu):", derivativeSecTypes.size());
		for (unsigned int j = 0; j < derivativeSecTypes.size(); j++) {
			Scm_Printf(SCM_CURERR, " %s", derivativeSecTypes[j].c_str());
		}
		Scm_Printf(SCM_CURERR, "\n");
	}
}
//! [symbolSamples]

//! [mktDepthExchanges]
void GaucheAdapter::mktDepthExchanges(const std::vector<DepthMktDataDescription> &depthMktDataDescriptions) {
	Scm_Printf(SCM_CURERR, "Mkt Depth Exchanges (%lu):\n", depthMktDataDescriptions.size());

	for (unsigned int i = 0; i < depthMktDataDescriptions.size(); i++) {
		Scm_Printf(SCM_CURERR, "Depth Mkt Data Description [%d] - exchange: %s secType: %s listingExch: %s serviceDataType: %s aggGroup: %s\n", i, 
			depthMktDataDescriptions[i].exchange.c_str(), 
			depthMktDataDescriptions[i].secType.c_str(), 
			depthMktDataDescriptions[i].listingExch.c_str(), 
			depthMktDataDescriptions[i].serviceDataType.c_str(), 
			depthMktDataDescriptions[i].aggGroup != INT_MAX ? std::to_string(depthMktDataDescriptions[i].aggGroup).c_str() : "");
	}
}
//! [mktDepthExchanges]

//! [tickNews]
void GaucheAdapter::tickNews(int tickerId, time_t timeStamp, const std::string& providerCode, const std::string& articleId, const std::string& headline, const std::string& extraData) {
	Scm_Printf(SCM_CURERR, "News Tick. TickerId: %d, TimeStamp: %s, ProviderCode: %s, ArticleId: %s, Headline: %s, ExtraData: %s\n", tickerId, ctime(&(timeStamp /= 1000)), providerCode.c_str(), articleId.c_str(), headline.c_str(), extraData.c_str());
}
//! [tickNews]

//! [smartcomponents]]
void GaucheAdapter::smartComponents(int reqId, const SmartComponentsMap& theMap) {
	Scm_Printf(SCM_CURERR, "Smart components: (%lu):\n", theMap.size());

	for (SmartComponentsMap::const_iterator i = theMap.begin(); i != theMap.end(); i++) {
		Scm_Printf(SCM_CURERR, " bit number: %d exchange: %s exchange letter: %c\n", i->first, std::get<0>(i->second).c_str(), std::get<1>(i->second));
	}
}
//! [smartcomponents]

//! [tickReqParams]
void GaucheAdapter::tickReqParams(int tickerId, double minTick, const std::string& bboExchange, int snapshotPermissions) {
	Scm_Printf(SCM_CURERR, "tickerId: %d, minTick: %g, bboExchange: %s, snapshotPermissions: %u", tickerId, minTick, bboExchange.c_str(), snapshotPermissions);

	m_bboExchange = bboExchange;
}
//! [tickReqParams]

//! [newsProviders]
void GaucheAdapter::newsProviders(const std::vector<NewsProvider> &newsProviders) {
	Scm_Printf(SCM_CURERR, "News providers (%lu):\n", newsProviders.size());

	for (unsigned int i = 0; i < newsProviders.size(); i++) {
		Scm_Printf(SCM_CURERR, "News provider [%d] - providerCode: %s providerName: %s\n", i, newsProviders[i].providerCode.c_str(), newsProviders[i].providerName.c_str());
	}
}
//! [newsProviders]

//! [newsArticle]
void GaucheAdapter::newsArticle(int requestId, int articleType, const std::string& articleText) {
	Scm_Printf(SCM_CURERR, "News Article. Request Id: %d, Article Type: %d\n", requestId, articleType);
	if (articleType == 0) {
		Scm_Printf(SCM_CURERR, "News Article Text (text or html): %s\n", articleText.c_str());
	} else if (articleType == 1) {
		std::string path;
		#if defined(IB_WIN32)
			TCHAR s[200];
			GetCurrentDirectory(200, s);
			path = s + std::string("\\MST$06f53098.pdf");
		#elif defined(IB_POSIX)
			char s[1024];
			if (getcwd(s, sizeof(s)) == NULL) {
				Scm_Printf(SCM_CURERR, "getcwd() error\n");
				return;
			}
			path = s + std::string("/MST$06f53098.pdf");
		#endif
		std::vector<std::uint8_t> bytes = Utils::base64_decode(articleText);
		std::ofstream outfile(path, std::ios::out | std::ios::binary); 
		outfile.write((const char*)bytes.data(), bytes.size());
		Scm_Printf(SCM_CURERR, "Binary/pdf article was saved to: %s\n", path.c_str());
	}
}
//! [newsArticle]

//! [historicalNews]
void GaucheAdapter::historicalNews(int requestId, const std::string& time, const std::string& providerCode, const std::string& articleId, const std::string& headline) {
	Scm_Printf(SCM_CURERR, "Historical News. RequestId: %d, Time: %s, ProviderCode: %s, ArticleId: %s, Headline: %s\n", requestId, time.c_str(), providerCode.c_str(), articleId.c_str(), headline.c_str());
}
//! [historicalNews]

//! [historicalNewsEnd]
void GaucheAdapter::historicalNewsEnd(int requestId, bool hasMore) {
	Scm_Printf(SCM_CURERR, "Historical News End. RequestId: %d, HasMore: %s\n", requestId, (hasMore ? "true" : " false"));
}
//! [historicalNewsEnd]

//! [headTimestamp]
void GaucheAdapter::headTimestamp(int reqId, const std::string& headTimestamp) {
	Scm_Printf(SCM_CURERR, "Head time stamp. ReqId: %d - Head time stamp: %s,\n", reqId, headTimestamp.c_str());

}
//! [headTimestamp]

//! [histogramData]
void GaucheAdapter::histogramData(int reqId, const HistogramDataVector& data) {
	Scm_Printf(SCM_CURERR, "Histogram. ReqId: %d, data length: %lu\n", reqId, data.size());

	for (auto item : data) {
		Scm_Printf(SCM_CURERR, "\t price: %f, size: %lld\n", item.price, item.size);
	}
}
//! [histogramData]

//! [historicalDataUpdate]
void GaucheAdapter::historicalDataUpdate(TickerId reqId, const Bar& bar) {
	Scm_Printf(SCM_CURERR, "HistoricalDataUpdate. ReqId: %ld - Date: %s, Open: %g, High: %g, Low: %g, Close: %g, Volume: %lld, Count: %d, WAP: %g\n", reqId, bar.time.c_str(), bar.open, bar.high, bar.low, bar.close, bar.volume, bar.count, bar.wap);
}
//! [historicalDataUpdate]

//! [rerouteMktDataReq]
void GaucheAdapter::rerouteMktDataReq(int reqId, int conid, const std::string& exchange) {
	Scm_Printf(SCM_CURERR, "Re-route market data request. ReqId: %d, ConId: %d, Exchange: %s\n", reqId, conid, exchange.c_str());
}
//! [rerouteMktDataReq]

//! [rerouteMktDepthReq]
void GaucheAdapter::rerouteMktDepthReq(int reqId, int conid, const std::string& exchange) {
	Scm_Printf(SCM_CURERR, "Re-route market depth request. ReqId: %d, ConId: %d, Exchange: %s\n", reqId, conid, exchange.c_str());
}
//! [rerouteMktDepthReq]

//! [marketRule]
void GaucheAdapter::marketRule(int marketRuleId, const std::vector<PriceIncrement> &priceIncrements) {
	Scm_Printf(SCM_CURERR, "Market Rule Id: %d\n", marketRuleId);
	for (unsigned int i = 0; i < priceIncrements.size(); i++) {
		Scm_Printf(SCM_CURERR, "Low Edge: %g, Increment: %g\n", priceIncrements[i].lowEdge, priceIncrements[i].increment);
	}
}
//! [marketRule]

//! [pnl]
void GaucheAdapter::pnl(int reqId, double dailyPnL, double unrealizedPnL, double realizedPnL) {
	Scm_Printf(SCM_CURERR, "PnL. ReqId: %d, daily PnL: %g, unrealized PnL: %g, realized PnL: %g\n", reqId, dailyPnL, unrealizedPnL, realizedPnL);
}
//! [pnl]

//! [pnlsingle]
void GaucheAdapter::pnlSingle(int reqId, int pos, double dailyPnL, double unrealizedPnL, double realizedPnL, double value) {
	Scm_Printf(SCM_CURERR, "PnL Single. ReqId: %d, pos: %d, daily PnL: %g, unrealized PnL: %g, realized PnL: %g, value: %g\n", reqId, pos, dailyPnL, unrealizedPnL, realizedPnL, value);
}
//! [pnlsingle]

//! [historicalticks]
void GaucheAdapter::historicalTicks(int reqId, const std::vector<HistoricalTick>& ticks, bool done) {
    for (HistoricalTick tick : ticks) {
	std::time_t t = tick.time;
        std::cout << "Historical tick. ReqId: " << reqId << ", time: " << ctime(&t) << ", price: "<< tick.price << ", size: " << tick.size << std::endl;
    }
}
//! [historicalticks]

//! [historicalticksbidask]
void GaucheAdapter::historicalTicksBidAsk(int reqId, const std::vector<HistoricalTickBidAsk>& ticks, bool done) {
    for (HistoricalTickBidAsk tick : ticks) {
	std::time_t t = tick.time;
        std::cout << "Historical tick bid/ask. ReqId: " << reqId << ", time: " << ctime(&t) << ", price bid: "<< tick.priceBid <<
            ", price ask: "<< tick.priceAsk << ", size bid: " << tick.sizeBid << ", size ask: " << tick.sizeAsk <<
            ", bidPastLow: " << tick.tickAttribBidAsk.bidPastLow << ", askPastHigh: " << tick.tickAttribBidAsk.askPastHigh << std::endl;
    }
}
//! [historicalticksbidask]

//! [historicaltickslast]
void GaucheAdapter::historicalTicksLast(int reqId, const std::vector<HistoricalTickLast>& ticks, bool done) {
    for (HistoricalTickLast tick : ticks) {
	std::time_t t = tick.time;
        std::cout << "Historical tick last. ReqId: " << reqId << ", time: " << ctime(&t) << ", price: "<< tick.price <<
            ", size: " << tick.size << ", exchange: " << tick.exchange << ", special conditions: " << tick.specialConditions <<
            ", unreported: " << tick.tickAttribLast.unreported << ", pastLimit: " << tick.tickAttribLast.pastLimit << std::endl;
    }
}
//! [historicaltickslast]

//! [tickbytickalllast]
void GaucheAdapter::tickByTickAllLast(int reqId, int tickType, time_t time, double price, int size, const TickAttribLast& tickAttribLast, const std::string& exchange, const std::string& specialConditions) {
    Scm_Printf(SCM_CURERR, "Tick-By-Tick. ReqId: %d, TickType: %s, Time: %s, Price: %g, Size: %d, PastLimit: %d, Unreported: %d, Exchange: %s, SpecialConditions:%s\n", 
        reqId, (tickType == 1 ? "Last" : "AllLast"), ctime(&time), price, size, tickAttribLast.pastLimit, tickAttribLast.unreported, exchange.c_str(), specialConditions.c_str());
}
//! [tickbytickalllast]

//! [tickbytickbidask]
void GaucheAdapter::tickByTickBidAsk(int reqId, time_t time, double bidPrice, double askPrice, int bidSize, int askSize, const TickAttribBidAsk& tickAttribBidAsk) {
    Scm_Printf(SCM_CURERR, "Tick-By-Tick. ReqId: %d, TickType: BidAsk, Time: %s, BidPrice: %g, AskPrice: %g, BidSize: %d, AskSize: %d, BidPastLow: %d, AskPastHigh: %d\n", 
        reqId, ctime(&time), bidPrice, askPrice, bidSize, askSize, tickAttribBidAsk.bidPastLow, tickAttribBidAsk.askPastHigh);
}
//! [tickbytickbidask]

//! [tickbytickmidpoint]
void GaucheAdapter::tickByTickMidPoint(int reqId, time_t time, double midPoint) {
    Scm_Printf(SCM_CURERR, "Tick-By-Tick. ReqId: %d, TickType: MidPoint, Time: %s, MidPoint: %g\n", reqId, ctime(&time), midPoint);
}
//! [tickbytickmidpoint]

//! [orderbound]
void GaucheAdapter::orderBound(long long orderId, int apiClientId, int apiOrderId) {
    Scm_Printf(SCM_CURERR, "Order bound. OrderId: %lld, ApiClientId: %d, ApiOrderId: %d\n", orderId, apiClientId, apiOrderId);
}
//! [orderbound]

//! [completedorder]
void GaucheAdapter::completedOrder(const Contract& contract, const Order& order, const OrderState& orderState) {
	Scm_Printf(SCM_CURERR, "CompletedOrder. PermId: %ld, ParentPermId: %lld, Account: %s, Symbol: %s, SecType: %s, Exchange: %s:, Action: %s, OrderType: %s, TotalQty: %g, CashQty: %g, FilledQty: %g, "
		"LmtPrice: %g, AuxPrice: %g, Status: %s, CompletedTime: %s, CompletedStatus: %s\n", 
		order.permId, order.parentPermId == UNSET_LONG ? 0 : order.parentPermId, order.account.c_str(), contract.symbol.c_str(), contract.secType.c_str(), contract.exchange.c_str(), 
		order.action.c_str(), order.orderType.c_str(), order.totalQuantity, order.cashQty == UNSET_DOUBLE ? 0 : order.cashQty, order.filledQuantity, 
		order.lmtPrice, order.auxPrice, orderState.status.c_str(), orderState.completedTime.c_str(), orderState.completedStatus.c_str());
}
//! [completedorder]

//! [completedordersend]
void GaucheAdapter::completedOrdersEnd() {
	Scm_Printf(SCM_CURERR, "CompletedOrdersEnd\n");
}
//! [completedordersend]

// Local Variables:
// mode: c++
// tab-width: 4
// c-basic-offset: 4
// End:
