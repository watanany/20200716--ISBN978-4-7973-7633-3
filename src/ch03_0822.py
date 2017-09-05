#!/usr/bin/env python
# -*- coding: utf-8 -*-
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from IPython import embed

sns.set(color_codes=True)

# 各CSVファイルを読み込む
dau = pd.read_csv('section3-dau.csv')
dpu = pd.read_csv('section3-dpu.csv')
install = pd.read_csv('section3-install.csv')

# dauとdpuをinner-joinする
dau_install = pd.merge(dau, install, on=['user_id', 'app_name'])
# dau_installとdpuをouter-joinする
dau_install_payment = pd.merge(dau_install, dpu, on=['log_date', 'app_name', 'user_id'], how='outer')

# payment列のNaNを0に変換する
dau_install_payment['payment'].fillna(0, inplace=True)

# log_dateとinstall_dateを元にlog_month列とinstall_month列を生成する
dau_install_payment['log_month'] = dau_install_payment['log_date'].str.slice(0, 7)
dau_install_payment['install_month'] = dau_install_payment['install_date'].str.slice(0, 7)

# install_monthとlog_monthが同じかどうかで新規ユーザ(install)か既存ユーザ(existing)かを判別するためのラベル用の列を生成する
dau_install_payment['user_type'] = np.where(dau_install_payment['install_month'] == dau_install_payment['log_month'], 'install', 'existing')


# 積み上げ棒グラフ用にサブグラフを生成する
_fig, ax = plt.subplots()

# log_monthとuser_typeのピボットテーブルを生成する。集計方法はpaymentの合計
crosstab = pd.crosstab(dau_install_payment['log_month'], dau_install_payment['user_type'],
                       aggfunc=sum, values=dau_install_payment['payment'])

# ピボットテーブルを積み上げ棒グラフで描画する
crosstab.plot.bar(stacked=True, ax=ax)


# ヒストグラム様にサブグラフを生成する
_fig, ax = plt.subplots()

# payment > 0でuser_typeがinstallの行をlog_monthでグループ化
groups = dau_install_payment[dau_install_payment['payment'] > 0][dau_install_payment['user_type'] == 'install'].groupby('log_month')

# 各グループごとにヒストグラムとして描画する
binwidth = 2000
for title, group in groups:
    payments = group.groupby('user_id')['payment'].sum()
    payments.plot.hist(alpha=0.5, ax=ax,
                       bins=np.arange(min(payments), max(payments) + binwidth,  binwidth))

# グラフを表示する
plt.show()
