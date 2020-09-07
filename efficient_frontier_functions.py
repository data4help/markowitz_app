#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug 27 12:23:36 2020

@author: paulmora
"""

# %% Preliminaries
import numpy as np
import pandas as pd
import pandas_datareader.data as web
from pandas_datareader._utils import RemoteDataError
from tqdm import tqdm
import scipy.optimize as solver
import quandl


class MarkowitzReturns():

    def __init__(self, tickers, start_date, end_date):
        if isinstance(tickers, list):
            self.tickers = tickers
        else:
            self.tickers = [tickers]
        self.start_date = start_date
        self.end_date = end_date

        # Loading the returns
        self.returns = self.financial_dataframe_creation(self.tickers,
                                                         self.start_date,
                                                         self.end_date)

        # Get all possible portfolios
        (self.port_ret,
         self.port_std,
         self.stock_ret,
         self.stock_std,
         self.stock_cov,
         self.stock_corr,
         self.stock_weights,
         self.port_weights) = self.all_possible_portfolios(self.returns)

        # Get efficient frontier
        (self.ef_return,
         self.ef_std,
         self.ef_weights) = self.efficient_frontier(self.port_ret,
                                                    self.stock_ret,
                                                    self.stock_cov)

        # Pulling the risk free rate
        self.risk_free_rate = self.pulling_risk_free_rate(self.start_date,
                                                          self.end_date)

        # Sharpe ratio for stocks
        self.stock_sr = self.sharpe_ratio_calculation(self.stock_ret,
                                                      self.stock_std,
                                                      self.risk_free_rate)

        # Calculation of the sharpe ratio of the simulated portfolios
        self.sharpe_ratio = self.sharpe_ratio_calculation(self.port_ret,
                                                          self.port_std,
                                                          self.risk_free_rate)

        # Calculation of the sharpe ratio of efficient frontier portfolios
        self.ef_sharpe_ratio = self.sharpe_ratio_calculation(
            self.ef_return,
            self.ef_std,
            self.risk_free_rate)

        # Calculating the CML
        (self.cml,
         self.cml_xaxis,
         self.tangency_return,
         self.tangency_risk,
         self.tangency_sh) = self.capital_market_line(self.risk_free_rate,
                                                      self.ef_return,
                                                      self.ef_std)

        # Combining all results
        self.results = {
            "stocks": pd.DataFrame({
                "name": self.tickers,
                "return": self.stock_ret,
                "risk": self.stock_std,
                "sharpe_ratio": self.stock_sr
                }),
            "stock_weight": pd.DataFrame(
                self.stock_weights
                ),
            "stock_time_series": pd.DataFrame(
                self.returns
                ),
            "stock_correlation": self.stock_corr,
            "portfolio": pd.DataFrame({
                "return": self.port_ret,
                "risk": self.port_std,
                "sharpe_ratio": self.sharpe_ratio,
                }),
            "portfolio_weights": pd.DataFrame(
                self.port_weights
                ),
            "efficient_frontier": pd.DataFrame({
                "return": self.ef_return,
                "risk": self.ef_std,
                "sharpe_ratio": self.ef_sharpe_ratio
                }),
            "ef_weights": self.ef_weights,
            "cml": pd.DataFrame({
                "return": self.cml,
                "risk": self.cml_xaxis,
                "sharpe_ratio": len(self.cml) * list(self.tangency_sh)
                }),
            "tangency": pd.DataFrame({
                "return": self.tangency_return,
                "risk": self.tangency_risk,
                "sharpe_ratio": self.tangency_sh
                })
        }

    def financial_dataframe_creation(self,
                                     chosen_tickers,
                                     starting_date,
                                     ending_date):
        """
        This method pulls the price data for the given stock tickers.
        Furthermore, it is possible to specify for which starting and ending
        date would like to be chosen. Afterwards returns are calculated
        from these prices by using the log-approximation. In order
        to annualize returns, we multiply the resulting returns by
        the number of working days, namely 252

        Parameters
        ----------
        chosen_tickers : list
            list containing the stock tickers
        starting_date : string
            the starting date as a string
        ending_date : string
            the ending date as a string

        Returns
        -------
        returns : dataframe
            a dataframe containing the stock returns for the specified time

        """
        # Pull the chosen tickers
        df_prices = pd.DataFrame()
        for ticker in chosen_tickers:
            try:
                tmp = web.DataReader(ticker, "yahoo",
                                     starting_date,
                                     ending_date)
                df_prices.loc[:, ticker] = tmp['Adj Close']
            except KeyError:
                print("{} could not be loaded for this date".format(ticker))
                pass
            except RemoteDataError:
                print("{} could not be loaded, check your"
                      "internet connection".format(ticker))
                pass

        # Get annualized returns
        returns = np.log(df_prices / df_prices.shift(1)) * 252
        return returns

    def all_possible_portfolios(self, returns):
        """
        This method creates 10_000 possible portfolio combinations for
        the stocks given. This is done by creating 10_000 random
        numbers between 0 and 1 - meaning no short-selling allowed - and
        then calculates the risk and return for these portfolios

        Parameters
        ----------
        returns : dataframe
            a dataframe containing the annualized returns of the specified
            stocks

        Returns
        -------
        portfolio_returns : list
            a list containing the returns for all simulated portfolios
        portfolio_std : list
            a list containing the standard deviation for all simulated
            portfolios
        stock_returns : list
            a list containing the average returns for each individual stock
        stock_std : list
            a list containing the standard deviation for each individual
            stock
        stock_correlation : dataframe
            a dataframe stating the correlation between each individual stock

        """

        stock_returns = np.array(returns.mean().fillna(0))
        stock_covariance = np.array(returns.cov().fillna(0))
        stock_std = np.array(returns.std())
        stock_correlation = returns.corr()

        num_of_companies = len(returns.columns)
        number_of_weights = 10_000
        random_weights = np.random.dirichlet(np.ones(num_of_companies),
                                             size=number_of_weights)
        weight_times_covariance = np.matmul(random_weights,
                                            stock_covariance)
        weights_transpose = random_weights.T
        portfolio_returns = np.matmul(random_weights, stock_returns.T)

        portfolio_std = []
        for i in tqdm(range(number_of_weights)):
            row = weight_times_covariance[i, :]
            col = weights_transpose[:, i]
            diagonal_element = np.dot(row, col)
            element_std = np.sqrt(diagonal_element)
            portfolio_std.append(element_std)

        stock_weights = np.eye(len(returns.columns))

        return (portfolio_returns, portfolio_std, stock_returns,
                stock_std, stock_covariance, stock_correlation,
                stock_weights, random_weights)

    def efficient_frontier(self, portfolio_returns,
                           vector_returns, covariance_matrix):
        """
        This method calculates the efficient frontier for the simulated
        portfolios. This is done by stating the economic problem of minimizing
        the portfolios risk for a given return. Furthermore, several
        conditions are stated and the entirety is given to scipy to solve.
        Afterwards only those portfolios are picked which have a minimum
        return as the minimum variance portfolio.
        The method then returns the return and risk for the efficient frontier

        Parameters
        ----------
        portfolio_returns : list
            a list with the simulated portfolio returns
        vector_returns : list
            a list with the time-series averages of the different stock returns
        covariance_matrix : array
            a numpy array containing the covariance between the different
            stocks

        Returns
        -------
        given_return : list
            a list with the returns of the efficient frontier
        minimum_risk_given_return : list
            a list with the risk levels of the efficient frontier

        """
        num_of_companies = len(vector_returns)
        bounds = tuple((0, 1) for x in range(num_of_companies))
        initial_weights = np.random.dirichlet(np.ones(num_of_companies),
                                              size=1)
        min_return, max_return = min(portfolio_returns), max(portfolio_returns)
        given_return = np.arange(min_return, max_return, .001)
        objective_function = lambda x: np.sqrt(np.linalg.multi_dot(
            [x, covariance_matrix, x.T]))

        minimum_risk_given_return = []
        ef_weights = []
        for i in given_return:
            constraints = [{"type": "eq",
                            "fun": lambda x: sum(x) - 1},
                           {"type": "eq",
                            "fun": lambda x: (sum(x*vector_returns) - i)}]
            outcome = solver.minimize(objective_function,
                                      x0=initial_weights,
                                      method="SLSQP",
                                      constraints=constraints, bounds=bounds)
            minimum_risk_given_return.append(outcome.fun)
            ef_weights.append(list(outcome.x))

        # storing the mvp information
        mvp_risk = min(minimum_risk_given_return)
        mvp_return = given_return[minimum_risk_given_return == mvp_risk]

        # storing the efficient frontier information
        bool_ef = given_return >= mvp_return
        ef_return = given_return[bool_ef]
        ef_risk = np.array(minimum_risk_given_return)[bool_ef]
        all_weights = pd.DataFrame(np.array(ef_weights)[bool_ef])

        return (ef_return, ef_risk, all_weights)

    def pulling_risk_free_rate(self, start_date, end_date):
        """
        This method is extracting the risk free rate for a specified
        time-frame from quandl. The risk-free rate here is approximated
        by the one-month treasury bill from the United States. Afterwards,
        an annualized version is calculated.

        Parameters
        ----------
        start_date : string
            a string with the desired starting date of the time-series
        end_date : string
            a string with the desired ending date of the time-series

        Returns
        -------
        annualized_risk_free_rate : float
            the annualized risk-free rate

        """
        quandl_data = quandl.get("USTREASURY/BILLRATES",
                                 authtoken="GxoUoLmANtEtV2GmdxhV",
                                 start_date=start_date,
                                 end_date=end_date)
        risk_free_rate = quandl_data.loc[:, "4 Wk Bank Discount Rate"]
        average_rate = np.mean(risk_free_rate) / 100
        annualized_risk_free_rate = (1 + average_rate)**12 - 1

        return annualized_risk_free_rate

    def sharpe_ratio_calculation(self, port_returns, port_risks,
                                 risk_free_rate):
        """
        This method calculates the sharpe ratios for two lists of returns and
        risks. For this calculation the risk free rate is needed.

        Parameters
        ----------
        port_returns : list
            a list with portfolio returns
        port_risks : list
            a list with portfolio risks
        risk_free_rate : float
            a float with the annualized risk-free rate

        Returns
        -------
        sharpe_ratio : list
            a list with sharpe ratio

        """
        sharpe_ratio = ((port_returns-risk_free_rate)/port_risks)
        return sharpe_ratio

    def capital_market_line(self, risk_free_rate, ef_returns, ef_risks):
        """
        This method calculates the Capital Market Line of given set
        of simulated portfolios. This is done by a simple linear equation.
        For that we first find the point on the efficient frontier with
        the highest sharpe ratio. Afterwards the risk-free rate and the
        highest sharpe ration point are connected.

        Parameters
        ----------
        risk_free_rate : float
            a float stating the risk-free rate
        ef_returns : list
            a list containing the returns from the efficient frontier
        ef_risks : list
            a list containing the risk-levels from the efficient frontier

        Returns
        -------
        cml : list
            a list with the cml yaxis values
        cml_xaxis : TYPE
            a list with the cml xaxis values

        """
        ef_sharpe_ratio = (ef_returns-risk_free_rate) / ef_risks

        highest_sharpe_ratio = max(ef_sharpe_ratio)
        bool_tangency = ef_sharpe_ratio == highest_sharpe_ratio
        tangency_return = np.array(ef_returns)[bool_tangency]
        tangency_risk = np.array(ef_risks)[bool_tangency]
        tangency_sharpe_ratio = np.array(ef_sharpe_ratio)[bool_tangency]

        cml_xaxis = np.arange(0, max(ef_risks), 0.05)
        cml = risk_free_rate + highest_sharpe_ratio * cml_xaxis

        return (cml, cml_xaxis, tangency_return,
                tangency_risk, tangency_sharpe_ratio)
