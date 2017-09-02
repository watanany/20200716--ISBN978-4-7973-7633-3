#!/usr/bin/env python
# -*- coding: utf-8 -*-
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from IPython import embed

dau = pd.read_csv('section3-dau.csv')
dpu = pd.read_csv('section3-dpu.csv')
install = pd.read_csv('section3-install.csv')

dau_install = pd.merge(dau, install, on=['user_id', 'app_name'])
dau_install_payment = pd.merge(dau_install, dpu, on=['log_date', 'app_name', 'user_id'], how='outer')

dau_install_payment['payment'].fillna(0, inplace=True)

dau_install_payment['log_month'] = dau_install_payment['log_date'].str.slice(0, 7)
dau_install_payment['install_month'] = dau_install_payment['install_date'].str.slice(0, 7)
dau_install_payment['user_type'] = np.where(dau_install_payment['install_month'] == dau_install_payment['log_month'], 'install', 'existing')


_fig, ax = plt.subplots()
crosstab = pd.crosstab(dau_install_payment['log_month'], dau_install_payment['user_type'],
                       aggfunc=sum, values=dau_install_payment['payment'])
crosstab.plot.bar(stacked=True, ax=ax)


_fig, ax = plt.subplots()
groups = dau_install_payment.groupby(['log_month'])
for i, (title, group) in enumerate(groups):
    group = group[group['payment'] > 0]
    group['payment'].plot.hist(alpha=0.5, ax=ax)

plt.show()
