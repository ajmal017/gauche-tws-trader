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

    def start(self):
        quantity = 100000
        order = Order()
        order.action = "BUY"
        order.orderType = "MKT"
        order.totalQuantity = quantity

        self.orderId = self.nextOrderId()
        self.placeOrder(self.orderId, self.contract(), order)

        print("started")

    def orderStatus(self, orderId:OrderId , status:str, filled:float,
                    remaining:float, avgFillPrice:float, permId:int,
                    parentId:int, lastFillPrice:float, clientId:int,
                    whyHeld:str, mktCapPrice: float):
        print("orderStatus", orderId, status, filled, remaining, avgFillPrice)
        if orderId == self.orderId and remaining == 0 and self.stopOrderId == 0:
            self.stopOrderId = self.nextOrderId()
            price = avgFillPrice - 0.0003
            # to avoid the error "The price does not conform to the minimum price variation
            # for this contract" round the price in 0.5 pip
            price = math.floor(price / 0.00005) * 0.00005
            print("price", price)
            quantity = 100000
            order = Order()
            order.action = "SELL"
            order.orderType = "STP"
            order.auxPrice = price
            order.totalQuantity = quantity
            self.placeOrder(self.stopOrderId, self.contract(), order)

        elif orderId == self.stopOrderId:
            self.disconnect()

def main():
    app = TestApp()
    app.connect("127.0.0.1", 7497, clientId=0)
    app.run()

if __name__ == "__main__":
    main()
