import argparse
import datetime
import collections
import inspect
import math

import logging
import time
import os.path

from ibapi import wrapper
from ibapi import utils
from ibapi.client import EClient
from ibapi.utils import iswrapper

# types
from ibapi.common import * # @UnusedWildImport
from ibapi.order_condition import * # @UnusedWildImport
from ibapi.contract import * # @UnusedWildImport
from ibapi.order import * # @UnusedWildImport
from ibapi.order_state import * # @UnusedWildImport
from ibapi.execution import Execution
from ibapi.execution import ExecutionFilter
from ibapi.commission_report import CommissionReport
from ibapi.ticktype import * # @UnusedWildImport
from ibapi.tag_value import TagValue

from ibapi.account_summary_tags import *

class TestApp(wrapper.EWrapper, EClient):
    def __init__(self):
        EClient.__init__(self, wrapper=self)
        self.orderId = 0
        self.stopOrderId = 0

    def connectAck(self):
        if self.asynchronous:
            self.startApi()

    def managedAccounts(self, accountsList: str):
        print("Account list:", accountsList)
        self.account = accountsList.split(",")[0]

    def nextValidId(self, orderId: int):
        super().nextValidId(orderId)

        logging.debug("setting nextValidOrderId: %d", orderId)
        self.nextValidOrderId = orderId
        print("NextValidId:", orderId)

        # we can start now
        self.start()

    def nextOrderId(self):
        oid = self.nextValidOrderId
        self.nextValidOrderId += 1
        return oid

    def contract(self):
        contract = Contract()
        contract.symbol = "EUR"
        contract.secType = "CASH"
        contract.currency = "USD"
        contract.exchange = "IDEALPRO"
        return contract

    def EurUsd(self):
        #! [cashcontract]
        contract = Contract()
        contract.symbol = "EUR"
        contract.secType = "CASH"
        contract.currency = "USD"
        contract.exchange = "IDEALPRO"
        #! [cashcontract]
        return contract

    def start(self):
        queryTime = (datetime.datetime.today() - datetime.timedelta(days=180)).strftime("%Y%m%d %H:%M:%S")
        self.reqHistoricalData(4102, self.EurUsd(), queryTime,
                               "1 M", "1 hour", "MIDPOINT", 1, 1, False, [])

        print("started")

    def historicalData(self, reqId:int, bar: BarData):
        print("HistoricalData. ReqId:", reqId, "BarData.", bar)

def main():
    app = TestApp()
    app.connect("127.0.0.1", 7497, clientId=0)
    app.run()

if __name__ == "__main__":
    main()
