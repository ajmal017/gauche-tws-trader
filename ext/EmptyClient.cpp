/* Copyright (C) 2019 Interactive Brokers LLC. All rights reserved. This code is subject to the terms
 * and conditions of the IB API Non-Commercial License or the IB API Commercial License, as applicable. */

#include "StdAfx.h"

#include "EmptyClient.h"

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

const int PING_DEADLINE = 2; // seconds
const int SLEEP_BETWEEN_PINGS = 30; // seconds

///////////////////////////////////////////////////////////
// member funcs
//! [socket_init]
EmptyClient::EmptyClient() :
      m_osSignal(2000)//2-seconds timeout
    , m_pClient(new EClientSocket(this, &m_osSignal))
	, m_state(ST_CONNECT)
	, m_sleepDeadline(0)
	, m_orderId(0)
    , m_pReader(0)
    , m_extraAuth(false)
{
}
//! [socket_init]
EmptyClient::~EmptyClient()
{
    if (m_pReader)
        delete m_pReader;

    delete m_pClient;
}

bool EmptyClient::connect(const char *host, int port, int clientId)
{
	// trying to connect
	printf( "Connecting to %s:%d clientId:%d\n", !( host && *host) ? "127.0.0.1" : host, port, clientId);
	
	//! [connect]
	bool bRes = m_pClient->eConnect( host, port, clientId, m_extraAuth);
	//! [connect]
	
	if (bRes) {
		printf( "Connected to %s:%d clientId:%d\n", m_pClient->host().c_str(), m_pClient->port(), clientId);
		//! [ereader]
        m_pReader = new EReader(m_pClient, &m_osSignal);
		m_pReader->start();
		//! [ereader]
	}
	else
		printf( "Cannot connect to %s:%d clientId:%d\n", m_pClient->host().c_str(), m_pClient->port(), clientId);

	return bRes;
}

void EmptyClient::disconnect() const
{
	m_pClient->eDisconnect();

	printf ( "Disconnected\n");
}

bool EmptyClient::isConnected() const
{
	return m_pClient->isConnected();
}

void EmptyClient::setConnectOptions(const std::string& connectOptions)
{
	m_pClient->setConnectOptions(connectOptions);
}

void EmptyClient::processMessages()
{
	time_t now = time(NULL);

	/*****************************************************************/
    /* Below are few quick-to-test examples on the IB API functions grouped by functionality. Uncomment the relevant methods. */
    /*****************************************************************/
	switch (m_state) {
		case ST_PNLSINGLE:
			pnlSingleOperation();
			break;
		case ST_PNLSINGLE_ACK:
			break;
		case ST_PNL:
			pnlOperation();
			break;
		case ST_PNL_ACK:
			break;
		case ST_TICKDATAOPERATION:
			tickDataOperation();
			break;
		case ST_TICKDATAOPERATION_ACK:
			break;
		case ST_TICKOPTIONCOMPUTATIONOPERATION:
			tickOptionComputationOperation();
			break;
		case ST_TICKOPTIONCOMPUTATIONOPERATION_ACK:
			break;
		case ST_DELAYEDTICKDATAOPERATION:
			delayedTickDataOperation();
			break;
		case ST_DELAYEDTICKDATAOPERATION_ACK:
			break;
		case ST_MARKETDEPTHOPERATION:
			marketDepthOperations();
			break;
		case ST_MARKETDEPTHOPERATION_ACK:
			break;
		case ST_REALTIMEBARS:
			realTimeBars();
			break;
		case ST_REALTIMEBARS_ACK:
			break;
		case ST_MARKETDATATYPE:
			marketDataType();
			break;
		case ST_MARKETDATATYPE_ACK:
			break;
		case ST_HISTORICALDATAREQUESTS:
			historicalDataRequests();
			break;
		case ST_HISTORICALDATAREQUESTS_ACK:
			break;
		case ST_OPTIONSOPERATIONS:
			optionsOperations();
			break;
		case ST_OPTIONSOPERATIONS_ACK:
			break;
		case ST_CONTRACTOPERATION:
			contractOperations();
			break;
		case ST_CONTRACTOPERATION_ACK:
			break;
		case ST_MARKETSCANNERS:
			marketScanners();
			break;
		case ST_MARKETSCANNERS_ACK:
			break;
		case ST_FUNDAMENTALS:
			fundamentals();
			break;
		case ST_FUNDAMENTALS_ACK:
			break;
		case ST_BULLETINS:
			bulletins();
			break;
		case ST_BULLETINS_ACK:
			break;
		case ST_ACCOUNTOPERATIONS:
			accountOperations();
			break;
		case ST_ACCOUNTOPERATIONS_ACK:
			break;
		case ST_ORDEROPERATIONS:
			orderOperations();
			break;
		case ST_ORDEROPERATIONS_ACK:
			break;
		case ST_OCASAMPLES:
			ocaSamples();
			break;
		case ST_OCASAMPLES_ACK:
			break;
		case ST_CONDITIONSAMPLES:
			conditionSamples();
			break;
		case ST_CONDITIONSAMPLES_ACK:
			break;
		case ST_BRACKETSAMPLES:
			bracketSample();
			break;
		case ST_BRACKETSAMPLES_ACK:
			break;
		case ST_HEDGESAMPLES:
			hedgeSample();
			break;
		case ST_HEDGESAMPLES_ACK:
			break;
		case ST_TESTALGOSAMPLES:
			testAlgoSamples();
			break;
		case ST_TESTALGOSAMPLES_ACK:
			break;
		case ST_FAORDERSAMPLES:
			financialAdvisorOrderSamples();
			break;
		case ST_FAORDERSAMPLES_ACK:
			break;
		case ST_FAOPERATIONS:
			financialAdvisorOperations();
			break;
		case ST_FAOPERATIONS_ACK:
			break;
		case ST_DISPLAYGROUPS:
			testDisplayGroups();
			break;
		case ST_DISPLAYGROUPS_ACK:
			break;
		case ST_MISCELANEOUS:
			miscelaneous();
			break;
		case ST_MISCELANEOUS_ACK:
			break;
		case ST_FAMILYCODES:
			reqFamilyCodes();
			break;
		case ST_FAMILYCODES_ACK:
			break;
		case ST_SYMBOLSAMPLES:
			reqMatchingSymbols();
			break;
		case ST_SYMBOLSAMPLES_ACK:
			break;
		case ST_REQMKTDEPTHEXCHANGES:
			reqMktDepthExchanges();
			break;
		case ST_REQMKTDEPTHEXCHANGES_ACK:
			break;
		case ST_REQNEWSTICKS:
			reqNewsTicks();
			break;
		case ST_REQNEWSTICKS_ACK:
			break;
		case ST_REQSMARTCOMPONENTS:
			reqSmartComponents();
			break;
		case ST_REQSMARTCOMPONENTS_ACK:
			break;
		case ST_NEWSPROVIDERS:
			reqNewsProviders();
			break;
		case ST_NEWSPROVIDERS_ACK:
			break;
		case ST_REQNEWSARTICLE:
			reqNewsArticle();
			break;
		case ST_REQNEWSARTICLE_ACK:
			break;
		case ST_REQHISTORICALNEWS:
			reqHistoricalNews();
			break;
		case ST_REQHISTORICALNEWS_ACK:
			break;
		case ST_REQHEADTIMESTAMP:
			reqHeadTimestamp();
			break;
		case ST_REQHISTOGRAMDATA:
			reqHistogramData();
			break;
		case ST_REROUTECFD:
			rerouteCFDOperations();
			break;
		case ST_MARKETRULE:
			marketRuleOperations();
			break;
		case ST_CONTFUT:
			continuousFuturesOperations();
			break;
        case ST_REQHISTORICALTICKS:
            reqHistoricalTicks();
            break;
        case ST_REQHISTORICALTICKS_ACK:
            break;
		case ST_REQTICKBYTICKDATA:
			reqTickByTickData();
			break;
		case ST_REQTICKBYTICKDATA_ACK:
			break;
		case ST_WHATIFSAMPLES:
			whatIfSamples();
			break;
		case ST_WHATIFSAMPLES_ACK:
			break;
		case ST_PING:
			reqCurrentTime();
			break;
		case ST_PING_ACK:
			if( m_sleepDeadline < now) {
				disconnect();
				return;
			}
			break;
		case ST_IDLE:
			if( m_sleepDeadline < now) {
				m_state = ST_PING;
				return;
			}
			break;
	}

	m_osSignal.waitForSignal();
	errno = 0;
	m_pReader->processMsgs();
}

//////////////////////////////////////////////////////////////////
// methods
//! [connectack]
void EmptyClient::connectAck() {
	if (!m_extraAuth && m_pClient->asyncEConnect())
        m_pClient->startApi();
}
//! [connectack]

void EmptyClient::reqCurrentTime() {}

void EmptyClient::pnlOperation() {}

void EmptyClient::pnlSingleOperation() {}

void EmptyClient::tickDataOperation() {}

void EmptyClient::tickOptionComputationOperation() {}

void EmptyClient::delayedTickDataOperation() {}

void EmptyClient::marketDepthOperations() {}

void EmptyClient::realTimeBars() {}

void EmptyClient::marketDataType() {}

void EmptyClient::historicalDataRequests() {}

void EmptyClient::optionsOperations() {}

void EmptyClient::contractOperations() {}

void EmptyClient::marketScanners() {}

void EmptyClient::fundamentals() {}

void EmptyClient::bulletins() {}

void EmptyClient::accountOperations() {}

void EmptyClient::orderOperations() {}

void EmptyClient::ocaSamples() {}

void EmptyClient::conditionSamples() {}

void EmptyClient::bracketSample() {}

void EmptyClient::hedgeSample() {}

void EmptyClient::testAlgoSamples() {}

void EmptyClient::financialAdvisorOrderSamples() {}

void EmptyClient::financialAdvisorOperations() {}

void EmptyClient::testDisplayGroups() {}

void EmptyClient::miscelaneous() {}

void EmptyClient::reqFamilyCodes() {}

void EmptyClient::reqMatchingSymbols() {}

void EmptyClient::reqMktDepthExchanges() {}

void EmptyClient::reqNewsTicks() {}

void EmptyClient::reqSmartComponents() {}

void EmptyClient::reqNewsProviders() {}

void EmptyClient::reqNewsArticle() {}

void EmptyClient::reqHistoricalNews() {}

void EmptyClient::reqHeadTimestamp() {}

void EmptyClient::reqHistogramData() {}

void EmptyClient::rerouteCFDOperations() {}

void EmptyClient::marketRuleOperations() {}

void EmptyClient::continuousFuturesOperations() {}

void EmptyClient::reqHistoricalTicks()  {}

void EmptyClient::reqTickByTickData() {}

void EmptyClient::whatIfSamples() {}

//! [nextvalidid]
void EmptyClient::nextValidId( OrderId orderId)
{
	printf("Next Valid Id: %ld\n", orderId);
	m_orderId = orderId;
	//! [nextvalidid]
}


void EmptyClient::currentTime( long time) {}

//! [error]
void EmptyClient::error(int id, int errorCode, const std::string& errorString)
{
	printf( "Error. Id: %d, Code: %d, Msg: %s\n", id, errorCode, errorString.c_str());
}
//! [error]

//! [tickprice]
void EmptyClient::tickPrice( TickerId tickerId, TickType field, double price, const TickAttrib& attribs) {
	printf( "Tick Price. Ticker Id: %ld, Field: %d, Price: %g, CanAutoExecute: %d, PastLimit: %d, PreOpen: %d\n", tickerId, (int)field, price, attribs.canAutoExecute, attribs.pastLimit, attribs.preOpen);
}
//! [tickprice]

//! [ticksize]
void EmptyClient::tickSize( TickerId tickerId, TickType field, int size) {
	printf( "Tick Size. Ticker Id: %ld, Field: %d, Size: %d\n", tickerId, (int)field, size);
}
//! [ticksize]

//! [tickoptioncomputation]
void EmptyClient::tickOptionComputation( TickerId tickerId, TickType tickType, double impliedVol, double delta,
                                          double optPrice, double pvDividend,
                                          double gamma, double vega, double theta, double undPrice) {
	printf( "TickOptionComputation. Ticker Id: %ld, Type: %d, ImpliedVolatility: %g, Delta: %g, OptionPrice: %g, pvDividend: %g, Gamma: %g, Vega: %g, Theta: %g, Underlying Price: %g\n", tickerId, (int)tickType, impliedVol, delta, optPrice, pvDividend, gamma, vega, theta, undPrice);
}
//! [tickoptioncomputation]

//! [tickgeneric]
void EmptyClient::tickGeneric(TickerId tickerId, TickType tickType, double value) {
	printf( "Tick Generic. Ticker Id: %ld, Type: %d, Value: %g\n", tickerId, (int)tickType, value);
}
//! [tickgeneric]

//! [tickstring]
void EmptyClient::tickString(TickerId tickerId, TickType tickType, const std::string& value) {
	printf( "Tick String. Ticker Id: %ld, Type: %d, Value: %s\n", tickerId, (int)tickType, value.c_str());
}
//! [tickstring]

void EmptyClient::tickEFP(TickerId tickerId, TickType tickType, double basisPoints, const std::string& formattedBasisPoints,
                            double totalDividends, int holdDays, const std::string& futureLastTradeDate, double dividendImpact, double dividendsToLastTradeDate) {
	printf( "TickEFP. %ld, Type: %d, BasisPoints: %g, FormattedBasisPoints: %s, Total Dividends: %g, HoldDays: %d, Future Last Trade Date: %s, Dividend Impact: %g, Dividends To Last Trade Date: %g\n", tickerId, (int)tickType, basisPoints, formattedBasisPoints.c_str(), totalDividends, holdDays, futureLastTradeDate.c_str(), dividendImpact, dividendsToLastTradeDate);
}

//! [orderstatus]
void EmptyClient::orderStatus(OrderId orderId, const std::string& status, double filled,
		double remaining, double avgFillPrice, int permId, int parentId,
		double lastFillPrice, int clientId, const std::string& whyHeld, double mktCapPrice){
	printf("OrderStatus. Id: %ld, Status: %s, Filled: %g, Remaining: %g, AvgFillPrice: %g, PermId: %d, LastFillPrice: %g, ClientId: %d, WhyHeld: %s, MktCapPrice: %g\n", orderId, status.c_str(), filled, remaining, avgFillPrice, permId, lastFillPrice, clientId, whyHeld.c_str(), mktCapPrice);
}
//! [orderstatus]

//! [openorder]
void EmptyClient::openOrder( OrderId orderId, const Contract& contract, const Order& order, const OrderState& orderState) {
	printf( "OpenOrder. PermId: %ld, ClientId: %ld, OrderId: %ld, Account: %s, Symbol: %s, SecType: %s, Exchange: %s:, Action: %s, OrderType:%s, TotalQty: %g, CashQty: %g, "
	"LmtPrice: %g, AuxPrice: %g, Status: %s\n", 
		order.permId, order.clientId, orderId, order.account.c_str(), contract.symbol.c_str(), contract.secType.c_str(), contract.exchange.c_str(), 
		order.action.c_str(), order.orderType.c_str(), order.totalQuantity, order.cashQty == UNSET_DOUBLE ? 0 : order.cashQty, order.lmtPrice, order.auxPrice, orderState.status.c_str());
}
//! [openorder]

//! [openorderend]
void EmptyClient::openOrderEnd() {
	printf( "OpenOrderEnd\n");
}
//! [openorderend]

void EmptyClient::winError( const std::string& str, int lastError) {}
void EmptyClient::connectionClosed() {
	printf( "Connection Closed\n");
}

//! [updateaccountvalue]
void EmptyClient::updateAccountValue(const std::string& key, const std::string& val,
                                       const std::string& currency, const std::string& accountName) {
	printf("UpdateAccountValue. Key: %s, Value: %s, Currency: %s, Account Name: %s\n", key.c_str(), val.c_str(), currency.c_str(), accountName.c_str());
}
//! [updateaccountvalue]

//! [updateportfolio]
void EmptyClient::updatePortfolio(const Contract& contract, double position,
                                    double marketPrice, double marketValue, double averageCost,
                                    double unrealizedPNL, double realizedPNL, const std::string& accountName){
	printf("UpdatePortfolio. %s, %s @ %s: Position: %g, MarketPrice: %g, MarketValue: %g, AverageCost: %g, UnrealizedPNL: %g, RealizedPNL: %g, AccountName: %s\n", (contract.symbol).c_str(), (contract.secType).c_str(), (contract.primaryExchange).c_str(), position, marketPrice, marketValue, averageCost, unrealizedPNL, realizedPNL, accountName.c_str());
}
//! [updateportfolio]

//! [updateaccounttime]
void EmptyClient::updateAccountTime(const std::string& timeStamp) {
	printf( "UpdateAccountTime. Time: %s\n", timeStamp.c_str());
}
//! [updateaccounttime]

//! [accountdownloadend]
void EmptyClient::accountDownloadEnd(const std::string& accountName) {
	printf( "Account download finished: %s\n", accountName.c_str());
}
//! [accountdownloadend]

//! [contractdetails]
void EmptyClient::contractDetails( int reqId, const ContractDetails& contractDetails) {
	printf( "ContractDetails begin. ReqId: %d\n", reqId);
	printContractMsg(contractDetails.contract);
	printContractDetailsMsg(contractDetails);
	printf( "ContractDetails end. ReqId: %d\n", reqId);
}
//! [contractdetails]

//! [bondcontractdetails]
void EmptyClient::bondContractDetails( int reqId, const ContractDetails& contractDetails) {
	printf( "BondContractDetails begin. ReqId: %d\n", reqId);
	printBondContractDetailsMsg(contractDetails);
	printf( "BondContractDetails end. ReqId: %d\n", reqId);
}
//! [bondcontractdetails]

void EmptyClient::printContractMsg(const Contract& contract) {
	printf("\tConId: %ld\n", contract.conId);
	printf("\tSymbol: %s\n", contract.symbol.c_str());
	printf("\tSecType: %s\n", contract.secType.c_str());
	printf("\tLastTradeDateOrContractMonth: %s\n", contract.lastTradeDateOrContractMonth.c_str());
	printf("\tStrike: %g\n", contract.strike);
	printf("\tRight: %s\n", contract.right.c_str());
	printf("\tMultiplier: %s\n", contract.multiplier.c_str());
	printf("\tExchange: %s\n", contract.exchange.c_str());
	printf("\tPrimaryExchange: %s\n", contract.primaryExchange.c_str());
	printf("\tCurrency: %s\n", contract.currency.c_str());
	printf("\tLocalSymbol: %s\n", contract.localSymbol.c_str());
	printf("\tTradingClass: %s\n", contract.tradingClass.c_str());
}

void EmptyClient::printContractDetailsMsg(const ContractDetails& contractDetails) {
	printf("\tMarketName: %s\n", contractDetails.marketName.c_str());
	printf("\tMinTick: %g\n", contractDetails.minTick);
	printf("\tPriceMagnifier: %ld\n", contractDetails.priceMagnifier);
	printf("\tOrderTypes: %s\n", contractDetails.orderTypes.c_str());
	printf("\tValidExchanges: %s\n", contractDetails.validExchanges.c_str());
	printf("\tUnderConId: %d\n", contractDetails.underConId);
	printf("\tLongName: %s\n", contractDetails.longName.c_str());
	printf("\tContractMonth: %s\n", contractDetails.contractMonth.c_str());
	printf("\tIndystry: %s\n", contractDetails.industry.c_str());
	printf("\tCategory: %s\n", contractDetails.category.c_str());
	printf("\tSubCategory: %s\n", contractDetails.subcategory.c_str());
	printf("\tTimeZoneId: %s\n", contractDetails.timeZoneId.c_str());
	printf("\tTradingHours: %s\n", contractDetails.tradingHours.c_str());
	printf("\tLiquidHours: %s\n", contractDetails.liquidHours.c_str());
	printf("\tEvRule: %s\n", contractDetails.evRule.c_str());
	printf("\tEvMultiplier: %g\n", contractDetails.evMultiplier);
	printf("\tMdSizeMultiplier: %d\n", contractDetails.mdSizeMultiplier);
	printf("\tAggGroup: %d\n", contractDetails.aggGroup);
	printf("\tUnderSymbol: %s\n", contractDetails.underSymbol.c_str());
	printf("\tUnderSecType: %s\n", contractDetails.underSecType.c_str());
	printf("\tMarketRuleIds: %s\n", contractDetails.marketRuleIds.c_str());
	printf("\tRealExpirationDate: %s\n", contractDetails.realExpirationDate.c_str());
	printf("\tLastTradeTime: %s\n", contractDetails.lastTradeTime.c_str());
	printContractDetailsSecIdList(contractDetails.secIdList);
}

void EmptyClient::printContractDetailsSecIdList(const TagValueListSPtr &secIdList) {
	const int secIdListCount = secIdList.get() ? secIdList->size() : 0;
	if (secIdListCount > 0) {
		printf("\tSecIdList: {");
		for (int i = 0; i < secIdListCount; ++i) {
			const TagValue* tagValue = ((*secIdList)[i]).get();
			printf("%s=%s;",tagValue->tag.c_str(), tagValue->value.c_str());
		}
		printf("}\n");
	}
}

void EmptyClient::printBondContractDetailsMsg(const ContractDetails& contractDetails) {
	printf("\tSymbol: %s\n", contractDetails.contract.symbol.c_str());
	printf("\tSecType: %s\n", contractDetails.contract.secType.c_str());
	printf("\tCusip: %s\n", contractDetails.cusip.c_str());
	printf("\tCoupon: %g\n", contractDetails.coupon);
	printf("\tMaturity: %s\n", contractDetails.maturity.c_str());
	printf("\tIssueDate: %s\n", contractDetails.issueDate.c_str());
	printf("\tRatings: %s\n", contractDetails.ratings.c_str());
	printf("\tBondType: %s\n", contractDetails.bondType.c_str());
	printf("\tCouponType: %s\n", contractDetails.couponType.c_str());
	printf("\tConvertible: %s\n", contractDetails.convertible ? "yes" : "no");
	printf("\tCallable: %s\n", contractDetails.callable ? "yes" : "no");
	printf("\tPutable: %s\n", contractDetails.putable ? "yes" : "no");
	printf("\tDescAppend: %s\n", contractDetails.descAppend.c_str());
	printf("\tExchange: %s\n", contractDetails.contract.exchange.c_str());
	printf("\tCurrency: %s\n", contractDetails.contract.currency.c_str());
	printf("\tMarketName: %s\n", contractDetails.marketName.c_str());
	printf("\tTradingClass: %s\n", contractDetails.contract.tradingClass.c_str());
	printf("\tConId: %ld\n", contractDetails.contract.conId);
	printf("\tMinTick: %g\n", contractDetails.minTick);
	printf("\tMdSizeMultiplier: %d\n", contractDetails.mdSizeMultiplier);
	printf("\tOrderTypes: %s\n", contractDetails.orderTypes.c_str());
	printf("\tValidExchanges: %s\n", contractDetails.validExchanges.c_str());
	printf("\tNextOptionDate: %s\n", contractDetails.nextOptionDate.c_str());
	printf("\tNextOptionType: %s\n", contractDetails.nextOptionType.c_str());
	printf("\tNextOptionPartial: %s\n", contractDetails.nextOptionPartial ? "yes" : "no");
	printf("\tNotes: %s\n", contractDetails.notes.c_str());
	printf("\tLong Name: %s\n", contractDetails.longName.c_str());
	printf("\tEvRule: %s\n", contractDetails.evRule.c_str());
	printf("\tEvMultiplier: %g\n", contractDetails.evMultiplier);
	printf("\tAggGroup: %d\n", contractDetails.aggGroup);
	printf("\tMarketRuleIds: %s\n", contractDetails.marketRuleIds.c_str());
	printf("\tTimeZoneId: %s\n", contractDetails.timeZoneId.c_str());
	printf("\tLastTradeTime: %s\n", contractDetails.lastTradeTime.c_str());
	printContractDetailsSecIdList(contractDetails.secIdList);
}

//! [contractdetailsend]
void EmptyClient::contractDetailsEnd( int reqId) {
	printf( "ContractDetailsEnd. %d\n", reqId);
}
//! [contractdetailsend]

//! [execdetails]
void EmptyClient::execDetails( int reqId, const Contract& contract, const Execution& execution) {
	printf( "ExecDetails. ReqId: %d - %s, %s, %s - %s, %ld, %g, %d\n", reqId, contract.symbol.c_str(), contract.secType.c_str(), contract.currency.c_str(), execution.execId.c_str(), execution.orderId, execution.shares, execution.lastLiquidity);
}
//! [execdetails]

//! [execdetailsend]
void EmptyClient::execDetailsEnd( int reqId) {
	printf( "ExecDetailsEnd. %d\n", reqId);
}
//! [execdetailsend]

//! [updatemktdepth]
void EmptyClient::updateMktDepth(TickerId id, int position, int operation, int side,
                                   double price, int size) {
	printf( "UpdateMarketDepth. %ld - Position: %d, Operation: %d, Side: %d, Price: %g, Size: %d\n", id, position, operation, side, price, size);
}
//! [updatemktdepth]

//! [updatemktdepthl2]
void EmptyClient::updateMktDepthL2(TickerId id, int position, const std::string& marketMaker, int operation,
                                     int side, double price, int size, bool isSmartDepth) {
	printf( "UpdateMarketDepthL2. %ld - Position: %d, Operation: %d, Side: %d, Price: %g, Size: %d, isSmartDepth: %d\n", id, position, operation, side, price, size, isSmartDepth);
}
//! [updatemktdepthl2]

//! [updatenewsbulletin]
void EmptyClient::updateNewsBulletin(int msgId, int msgType, const std::string& newsMessage, const std::string& originExch) {
	printf( "News Bulletins. %d - Type: %d, Message: %s, Exchange of Origin: %s\n", msgId, msgType, newsMessage.c_str(), originExch.c_str());
}
//! [updatenewsbulletin]

//! [managedaccounts]
void EmptyClient::managedAccounts( const std::string& accountsList) {
	printf( "Account List: %s\n", accountsList.c_str());
}
//! [managedaccounts]

//! [receivefa]
void EmptyClient::receiveFA(faDataType pFaDataType, const std::string& cxml) {
	std::cout << "Receiving FA: " << (int)pFaDataType << std::endl << cxml << std::endl;
}
//! [receivefa]

//! [historicaldata]
void EmptyClient::historicalData(TickerId reqId, const Bar& bar) {
	printf( "HistoricalData. ReqId: %ld - Date: %s, Open: %g, High: %g, Low: %g, Close: %g, Volume: %lld, Count: %d, WAP: %g\n", reqId, bar.time.c_str(), bar.open, bar.high, bar.low, bar.close, bar.volume, bar.count, bar.wap);
}
//! [historicaldata]

//! [historicaldataend]
void EmptyClient::historicalDataEnd(int reqId, const std::string& startDateStr, const std::string& endDateStr) {
	std::cout << "HistoricalDataEnd. ReqId: " << reqId << " - Start Date: " << startDateStr << ", End Date: " << endDateStr << std::endl;

    disconnect();
}
//! [historicaldataend]

//! [scannerparameters]
void EmptyClient::scannerParameters(const std::string& xml) {
	printf( "ScannerParameters. %s\n", xml.c_str());
}
//! [scannerparameters]

//! [scannerdata]
void EmptyClient::scannerData(int reqId, int rank, const ContractDetails& contractDetails,
                                const std::string& distance, const std::string& benchmark, const std::string& projection,
                                const std::string& legsStr) {
	printf( "ScannerData. %d - Rank: %d, Symbol: %s, SecType: %s, Currency: %s, Distance: %s, Benchmark: %s, Projection: %s, Legs String: %s\n", reqId, rank, contractDetails.contract.symbol.c_str(), contractDetails.contract.secType.c_str(), contractDetails.contract.currency.c_str(), distance.c_str(), benchmark.c_str(), projection.c_str(), legsStr.c_str());
}
//! [scannerdata]

//! [scannerdataend]
void EmptyClient::scannerDataEnd(int reqId) {
	printf( "ScannerDataEnd. %d\n", reqId);
}
//! [scannerdataend]

//! [realtimebar]
void EmptyClient::realtimeBar(TickerId reqId, long time, double open, double high, double low, double close,
                                long volume, double wap, int count) {
	printf( "RealTimeBars. %ld - Time: %ld, Open: %g, High: %g, Low: %g, Close: %g, Volume: %ld, Count: %d, WAP: %g\n", reqId, time, open, high, low, close, volume, count, wap);
}
//! [realtimebar]

//! [fundamentaldata]
void EmptyClient::fundamentalData(TickerId reqId, const std::string& data) {
	printf( "FundamentalData. ReqId: %ld, %s\n", reqId, data.c_str());
}
//! [fundamentaldata]

void EmptyClient::deltaNeutralValidation(int reqId, const DeltaNeutralContract& deltaNeutralContract) {
	printf( "DeltaNeutralValidation. %d, ConId: %ld, Delta: %g, Price: %g\n", reqId, deltaNeutralContract.conId, deltaNeutralContract.delta, deltaNeutralContract.price);
}

//! [ticksnapshotend]
void EmptyClient::tickSnapshotEnd(int reqId) {
	printf( "TickSnapshotEnd: %d\n", reqId);
}
//! [ticksnapshotend]

//! [marketdatatype]
void EmptyClient::marketDataType(TickerId reqId, int marketDataType) {
	printf( "MarketDataType. ReqId: %ld, Type: %d\n", reqId, marketDataType);
}
//! [marketdatatype]

//! [commissionreport]
void EmptyClient::commissionReport( const CommissionReport& commissionReport) {
	printf( "CommissionReport. %s - %g %s RPNL %g\n", commissionReport.execId.c_str(), commissionReport.commission, commissionReport.currency.c_str(), commissionReport.realizedPNL);
}
//! [commissionreport]

//! [position]
void EmptyClient::position( const std::string& account, const Contract& contract, double position, double avgCost) {
	printf( "Position. %s - Symbol: %s, SecType: %s, Currency: %s, Position: %g, Avg Cost: %g\n", account.c_str(), contract.symbol.c_str(), contract.secType.c_str(), contract.currency.c_str(), position, avgCost);
}
//! [position]

//! [positionend]
void EmptyClient::positionEnd() {
	printf( "PositionEnd\n");
}
//! [positionend]

//! [accountsummary]
void EmptyClient::accountSummary( int reqId, const std::string& account, const std::string& tag, const std::string& value, const std::string& currency) {
	printf( "Acct Summary. ReqId: %d, Account: %s, Tag: %s, Value: %s, Currency: %s\n", reqId, account.c_str(), tag.c_str(), value.c_str(), currency.c_str());
}
//! [accountsummary]

//! [accountsummaryend]
void EmptyClient::accountSummaryEnd( int reqId) {
	printf( "AccountSummaryEnd. Req Id: %d\n", reqId);
}
//! [accountsummaryend]

void EmptyClient::verifyMessageAPI( const std::string& apiData) {
	printf("verifyMessageAPI: %s\b", apiData.c_str());
}

void EmptyClient::verifyCompleted( bool isSuccessful, const std::string& errorText) {
	printf("verifyCompleted. IsSuccessfule: %d - Error: %s\n", isSuccessful, errorText.c_str());
}

void EmptyClient::verifyAndAuthMessageAPI( const std::string& apiDatai, const std::string& xyzChallenge) {
	printf("verifyAndAuthMessageAPI: %s %s\n", apiDatai.c_str(), xyzChallenge.c_str());
}

void EmptyClient::verifyAndAuthCompleted( bool isSuccessful, const std::string& errorText) {
	printf("verifyAndAuthCompleted. IsSuccessful: %d - Error: %s\n", isSuccessful, errorText.c_str());
    if (isSuccessful)
        m_pClient->startApi();
}

//! [displaygrouplist]
void EmptyClient::displayGroupList( int reqId, const std::string& groups) {
	printf("Display Group List. ReqId: %d, Groups: %s\n", reqId, groups.c_str());
}
//! [displaygrouplist]

//! [displaygroupupdated]
void EmptyClient::displayGroupUpdated( int reqId, const std::string& contractInfo) {
	std::cout << "Display Group Updated. ReqId: " << reqId << ", Contract Info: " << contractInfo << std::endl;
}
//! [displaygroupupdated]

//! [positionmulti]
void EmptyClient::positionMulti( int reqId, const std::string& account,const std::string& modelCode, const Contract& contract, double pos, double avgCost) {
	printf("Position Multi. Request: %d, Account: %s, ModelCode: %s, Symbol: %s, SecType: %s, Currency: %s, Position: %g, Avg Cost: %g\n", reqId, account.c_str(), modelCode.c_str(), contract.symbol.c_str(), contract.secType.c_str(), contract.currency.c_str(), pos, avgCost);
}
//! [positionmulti]

//! [positionmultiend]
void EmptyClient::positionMultiEnd( int reqId) {
	printf("Position Multi End. Request: %d\n", reqId);
}
//! [positionmultiend]

//! [accountupdatemulti]
void EmptyClient::accountUpdateMulti( int reqId, const std::string& account, const std::string& modelCode, const std::string& key, const std::string& value, const std::string& currency) {
	printf("AccountUpdate Multi. Request: %d, Account: %s, ModelCode: %s, Key, %s, Value: %s, Currency: %s\n", reqId, account.c_str(), modelCode.c_str(), key.c_str(), value.c_str(), currency.c_str());
}
//! [accountupdatemulti]

//! [accountupdatemultiend]
void EmptyClient::accountUpdateMultiEnd( int reqId) {
	printf("Account Update Multi End. Request: %d\n", reqId);
}
//! [accountupdatemultiend]

//! [securityDefinitionOptionParameter]
void EmptyClient::securityDefinitionOptionalParameter(int reqId, const std::string& exchange, int underlyingConId, const std::string& tradingClass,
                                                        const std::string& multiplier, const std::set<std::string>& expirations, const std::set<double>& strikes) {
	printf("Security Definition Optional Parameter. Request: %d, Trading Class: %s, Multiplier: %s\n", reqId, tradingClass.c_str(), multiplier.c_str());
}
//! [securityDefinitionOptionParameter]

//! [securityDefinitionOptionParameterEnd]
void EmptyClient::securityDefinitionOptionalParameterEnd(int reqId) {
	printf("Security Definition Optional Parameter End. Request: %d\n", reqId);
}
//! [securityDefinitionOptionParameterEnd]

//! [softDollarTiers]
void EmptyClient::softDollarTiers(int reqId, const std::vector<SoftDollarTier> &tiers) {
	printf("Soft dollar tiers (%lu):", tiers.size());

	for (unsigned int i = 0; i < tiers.size(); i++) {
		printf("%s\n", tiers[i].displayName().c_str());
	}
}
//! [softDollarTiers]

//! [familyCodes]
void EmptyClient::familyCodes(const std::vector<FamilyCode> &familyCodes) {
	printf("Family codes (%lu):\n", familyCodes.size());

	for (unsigned int i = 0; i < familyCodes.size(); i++) {
		printf("Family code [%d] - accountID: %s familyCodeStr: %s\n", i, familyCodes[i].accountID.c_str(), familyCodes[i].familyCodeStr.c_str());
	}
}
//! [familyCodes]

//! [symbolSamples]
void EmptyClient::symbolSamples(int reqId, const std::vector<ContractDescription> &contractDescriptions) {
	printf("Symbol Samples (total=%lu) reqId: %d\n", contractDescriptions.size(), reqId);

	for (unsigned int i = 0; i < contractDescriptions.size(); i++) {
		Contract contract = contractDescriptions[i].contract;
		std::vector<std::string> derivativeSecTypes = contractDescriptions[i].derivativeSecTypes;
		printf("Contract (%u): %ld %s %s %s %s, ", i, contract.conId, contract.symbol.c_str(), contract.secType.c_str(), contract.primaryExchange.c_str(), contract.currency.c_str());
		printf("Derivative Sec-types (%lu):", derivativeSecTypes.size());
		for (unsigned int j = 0; j < derivativeSecTypes.size(); j++) {
			printf(" %s", derivativeSecTypes[j].c_str());
		}
		printf("\n");
	}
}
//! [symbolSamples]

//! [mktDepthExchanges]
void EmptyClient::mktDepthExchanges(const std::vector<DepthMktDataDescription> &depthMktDataDescriptions) {
	printf("Mkt Depth Exchanges (%lu):\n", depthMktDataDescriptions.size());

	for (unsigned int i = 0; i < depthMktDataDescriptions.size(); i++) {
		printf("Depth Mkt Data Description [%d] - exchange: %s secType: %s listingExch: %s serviceDataType: %s aggGroup: %s\n", i, 
			depthMktDataDescriptions[i].exchange.c_str(), 
			depthMktDataDescriptions[i].secType.c_str(), 
			depthMktDataDescriptions[i].listingExch.c_str(), 
			depthMktDataDescriptions[i].serviceDataType.c_str(), 
			depthMktDataDescriptions[i].aggGroup != INT_MAX ? std::to_string(depthMktDataDescriptions[i].aggGroup).c_str() : "");
	}
}
//! [mktDepthExchanges]

//! [tickNews]
void EmptyClient::tickNews(int tickerId, time_t timeStamp, const std::string& providerCode, const std::string& articleId, const std::string& headline, const std::string& extraData) {
	printf("News Tick. TickerId: %d, TimeStamp: %s, ProviderCode: %s, ArticleId: %s, Headline: %s, ExtraData: %s\n", tickerId, ctime(&(timeStamp /= 1000)), providerCode.c_str(), articleId.c_str(), headline.c_str(), extraData.c_str());
}
//! [tickNews]

//! [smartcomponents]]
void EmptyClient::smartComponents(int reqId, const SmartComponentsMap& theMap) {
	printf("Smart components: (%lu):\n", theMap.size());

	for (SmartComponentsMap::const_iterator i = theMap.begin(); i != theMap.end(); i++) {
		printf(" bit number: %d exchange: %s exchange letter: %c\n", i->first, std::get<0>(i->second).c_str(), std::get<1>(i->second));
	}
}
//! [smartcomponents]

//! [tickReqParams]
void EmptyClient::tickReqParams(int tickerId, double minTick, const std::string& bboExchange, int snapshotPermissions) {
	printf("tickerId: %d, minTick: %g, bboExchange: %s, snapshotPermissions: %u", tickerId, minTick, bboExchange.c_str(), snapshotPermissions);

	m_bboExchange = bboExchange;
}
//! [tickReqParams]

//! [newsProviders]
void EmptyClient::newsProviders(const std::vector<NewsProvider> &newsProviders) {
	printf("News providers (%lu):\n", newsProviders.size());

	for (unsigned int i = 0; i < newsProviders.size(); i++) {
		printf("News provider [%d] - providerCode: %s providerName: %s\n", i, newsProviders[i].providerCode.c_str(), newsProviders[i].providerName.c_str());
	}
}
//! [newsProviders]

//! [newsArticle]
void EmptyClient::newsArticle(int requestId, int articleType, const std::string& articleText) {
	printf("News Article. Request Id: %d, Article Type: %d\n", requestId, articleType);
	if (articleType == 0) {
		printf("News Article Text (text or html): %s\n", articleText.c_str());
	} else if (articleType == 1) {
		std::string path;
		#if defined(IB_WIN32)
			TCHAR s[200];
			GetCurrentDirectory(200, s);
			path = s + std::string("\\MST$06f53098.pdf");
		#elif defined(IB_POSIX)
			char s[1024];
			if (getcwd(s, sizeof(s)) == NULL) {
				printf("getcwd() error\n");
				return;
			}
			path = s + std::string("/MST$06f53098.pdf");
		#endif
		std::vector<std::uint8_t> bytes = Utils::base64_decode(articleText);
		std::ofstream outfile(path, std::ios::out | std::ios::binary); 
		outfile.write((const char*)bytes.data(), bytes.size());
		printf("Binary/pdf article was saved to: %s\n", path.c_str());
	}
}
//! [newsArticle]

//! [historicalNews]
void EmptyClient::historicalNews(int requestId, const std::string& time, const std::string& providerCode, const std::string& articleId, const std::string& headline) {
	printf("Historical News. RequestId: %d, Time: %s, ProviderCode: %s, ArticleId: %s, Headline: %s\n", requestId, time.c_str(), providerCode.c_str(), articleId.c_str(), headline.c_str());
}
//! [historicalNews]

//! [historicalNewsEnd]
void EmptyClient::historicalNewsEnd(int requestId, bool hasMore) {
	printf("Historical News End. RequestId: %d, HasMore: %s\n", requestId, (hasMore ? "true" : " false"));
}
//! [historicalNewsEnd]

//! [headTimestamp]
void EmptyClient::headTimestamp(int reqId, const std::string& headTimestamp) {
	printf( "Head time stamp. ReqId: %d - Head time stamp: %s,\n", reqId, headTimestamp.c_str());

}
//! [headTimestamp]

//! [histogramData]
void EmptyClient::histogramData(int reqId, const HistogramDataVector& data) {
	printf("Histogram. ReqId: %d, data length: %lu\n", reqId, data.size());

	for (auto item : data) {
		printf("\t price: %f, size: %lld\n", item.price, item.size);
	}
}
//! [histogramData]

//! [historicalDataUpdate]
void EmptyClient::historicalDataUpdate(TickerId reqId, const Bar& bar) {
	printf( "HistoricalDataUpdate. ReqId: %ld - Date: %s, Open: %g, High: %g, Low: %g, Close: %g, Volume: %lld, Count: %d, WAP: %g\n", reqId, bar.time.c_str(), bar.open, bar.high, bar.low, bar.close, bar.volume, bar.count, bar.wap);
}
//! [historicalDataUpdate]

//! [rerouteMktDataReq]
void EmptyClient::rerouteMktDataReq(int reqId, int conid, const std::string& exchange) {
	printf( "Re-route market data request. ReqId: %d, ConId: %d, Exchange: %s\n", reqId, conid, exchange.c_str());
}
//! [rerouteMktDataReq]

//! [rerouteMktDepthReq]
void EmptyClient::rerouteMktDepthReq(int reqId, int conid, const std::string& exchange) {
	printf( "Re-route market depth request. ReqId: %d, ConId: %d, Exchange: %s\n", reqId, conid, exchange.c_str());
}
//! [rerouteMktDepthReq]

//! [marketRule]
void EmptyClient::marketRule(int marketRuleId, const std::vector<PriceIncrement> &priceIncrements) {
	printf("Market Rule Id: %d\n", marketRuleId);
	for (unsigned int i = 0; i < priceIncrements.size(); i++) {
		printf("Low Edge: %g, Increment: %g\n", priceIncrements[i].lowEdge, priceIncrements[i].increment);
	}
}
//! [marketRule]

//! [pnl]
void EmptyClient::pnl(int reqId, double dailyPnL, double unrealizedPnL, double realizedPnL) {
	printf("PnL. ReqId: %d, daily PnL: %g, unrealized PnL: %g, realized PnL: %g\n", reqId, dailyPnL, unrealizedPnL, realizedPnL);
}
//! [pnl]

//! [pnlsingle]
void EmptyClient::pnlSingle(int reqId, int pos, double dailyPnL, double unrealizedPnL, double realizedPnL, double value) {
	printf("PnL Single. ReqId: %d, pos: %d, daily PnL: %g, unrealized PnL: %g, realized PnL: %g, value: %g\n", reqId, pos, dailyPnL, unrealizedPnL, realizedPnL, value);
}
//! [pnlsingle]

//! [historicalticks]
void EmptyClient::historicalTicks(int reqId, const std::vector<HistoricalTick>& ticks, bool done) {
    for (HistoricalTick tick : ticks) {
	std::time_t t = tick.time;
        std::cout << "Historical tick. ReqId: " << reqId << ", time: " << ctime(&t) << ", price: "<< tick.price << ", size: " << tick.size << std::endl;
    }
}
//! [historicalticks]

//! [historicalticksbidask]
void EmptyClient::historicalTicksBidAsk(int reqId, const std::vector<HistoricalTickBidAsk>& ticks, bool done) {
    for (HistoricalTickBidAsk tick : ticks) {
	std::time_t t = tick.time;
        std::cout << "Historical tick bid/ask. ReqId: " << reqId << ", time: " << ctime(&t) << ", price bid: "<< tick.priceBid <<
            ", price ask: "<< tick.priceAsk << ", size bid: " << tick.sizeBid << ", size ask: " << tick.sizeAsk <<
            ", bidPastLow: " << tick.tickAttribBidAsk.bidPastLow << ", askPastHigh: " << tick.tickAttribBidAsk.askPastHigh << std::endl;
    }
}
//! [historicalticksbidask]

//! [historicaltickslast]
void EmptyClient::historicalTicksLast(int reqId, const std::vector<HistoricalTickLast>& ticks, bool done) {
    for (HistoricalTickLast tick : ticks) {
	std::time_t t = tick.time;
        std::cout << "Historical tick last. ReqId: " << reqId << ", time: " << ctime(&t) << ", price: "<< tick.price <<
            ", size: " << tick.size << ", exchange: " << tick.exchange << ", special conditions: " << tick.specialConditions <<
            ", unreported: " << tick.tickAttribLast.unreported << ", pastLimit: " << tick.tickAttribLast.pastLimit << std::endl;
    }
}
//! [historicaltickslast]

//! [tickbytickalllast]
void EmptyClient::tickByTickAllLast(int reqId, int tickType, time_t time, double price, int size, const TickAttribLast& tickAttribLast, const std::string& exchange, const std::string& specialConditions) {
    printf("Tick-By-Tick. ReqId: %d, TickType: %s, Time: %s, Price: %g, Size: %d, PastLimit: %d, Unreported: %d, Exchange: %s, SpecialConditions:%s\n", 
        reqId, (tickType == 1 ? "Last" : "AllLast"), ctime(&time), price, size, tickAttribLast.pastLimit, tickAttribLast.unreported, exchange.c_str(), specialConditions.c_str());
}
//! [tickbytickalllast]

//! [tickbytickbidask]
void EmptyClient::tickByTickBidAsk(int reqId, time_t time, double bidPrice, double askPrice, int bidSize, int askSize, const TickAttribBidAsk& tickAttribBidAsk) {
    printf("Tick-By-Tick. ReqId: %d, TickType: BidAsk, Time: %s, BidPrice: %g, AskPrice: %g, BidSize: %d, AskSize: %d, BidPastLow: %d, AskPastHigh: %d\n", 
        reqId, ctime(&time), bidPrice, askPrice, bidSize, askSize, tickAttribBidAsk.bidPastLow, tickAttribBidAsk.askPastHigh);
}
//! [tickbytickbidask]

//! [tickbytickmidpoint]
void EmptyClient::tickByTickMidPoint(int reqId, time_t time, double midPoint) {
    printf("Tick-By-Tick. ReqId: %d, TickType: MidPoint, Time: %s, MidPoint: %g\n", reqId, ctime(&time), midPoint);
}
//! [tickbytickmidpoint]

//! [orderbound]
void EmptyClient::orderBound(long long orderId, int apiClientId, int apiOrderId) {
    printf("Order bound. OrderId: %lld, ApiClientId: %d, ApiOrderId: %d\n", orderId, apiClientId, apiOrderId);
}
//! [orderbound]

//! [completedorder]
void EmptyClient::completedOrder(const Contract& contract, const Order& order, const OrderState& orderState) {
	printf( "CompletedOrder. PermId: %ld, ParentPermId: %lld, Account: %s, Symbol: %s, SecType: %s, Exchange: %s:, Action: %s, OrderType: %s, TotalQty: %g, CashQty: %g, FilledQty: %g, "
		"LmtPrice: %g, AuxPrice: %g, Status: %s, CompletedTime: %s, CompletedStatus: %s\n", 
		order.permId, order.parentPermId == UNSET_LONG ? 0 : order.parentPermId, order.account.c_str(), contract.symbol.c_str(), contract.secType.c_str(), contract.exchange.c_str(), 
		order.action.c_str(), order.orderType.c_str(), order.totalQuantity, order.cashQty == UNSET_DOUBLE ? 0 : order.cashQty, order.filledQuantity, 
		order.lmtPrice, order.auxPrice, orderState.status.c_str(), orderState.completedTime.c_str(), orderState.completedStatus.c_str());
}
//! [completedorder]

//! [completedordersend]
void EmptyClient::completedOrdersEnd() {
	printf( "CompletedOrdersEnd\n");
}
//! [completedordersend]
