import IPython as ipy
import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from datetime import date
from itertools import product
from collections import OrderedDict
from sklearn.cluster import KMeans
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler
from scipy import stats

def read_tsv_dates(base_dir, app_name, date_from, date_to):
    date_range = pd.date_range(date_from, date_to).strftime('%Y-%m-%d')
    paths = [os.path.join(base_dir, app_name, d, 'data.tsv') for d in date_range]
    data_frames = [pd.read_csv(p, sep='\t') for p in paths]
    return pd.concat(data_frames, ignore_index=True)

def read_dau(app_name, date_from, date_to):
    return read_tsv_dates('./sample-data/section8/daily/dau', app_name, date_from, date_to)

def read_dpu(app_name, date_from, date_to):
    return read_tsv_dates('./sample-data/section8/daily/dpu', app_name, date_from, date_to)

def read_action_daily(app_name, date_from, date_to):
    return read_tsv_dates('./sample-data/section8/daily/action', app_name, date_from, date_to)

def get_mau():
    date_from, date_to = date(2013, 5, 1), date(2013, 10, 31)
    dau = read_dau('game-01', date_from, date_to)
    dpu = read_dpu('game-01', date_from, date_to)
    dau = pd.merge(dau, dpu[['log_date', 'user_id', 'payment']], on=['log_date', 'user_id'], how='outer')
    dau['is_payment'] = np.where(dau['payment'].isnull(), False, True)
    dau['payment'] = dau['payment'].fillna(0)
    dau['log_month'] = pd.to_datetime(dau['log_date']).apply(lambda dt: dt.strftime('%Y-%m'))
    mau = pd.crosstab(dau['log_month'], dau['user_id'], aggfunc=sum, values=dau['payment'])
    return mau

def remove_near_zero_var(df, freqcut=(95 / 5), uniqcut=10):
    N = pd.DataFrame(StandardScaler().fit_transform(df.values), columns=df.columns)
    M = pd.DataFrame(
        [[mode_rate(N[c]), uniq_rate(N[c]) * 100, N[c].std() == 0] for c in N.columns],
        index=N.columns, columns=['freq_ratio', 'percent_unique', 'zero_var']
    )
    M.loc[:, 'nzv'] = M['zero_var'] | ((M['freq_ratio'] > freqcut) & (M['percent_unique'] < uniqcut))
    return df.drop(M[M['nzv'] == True].index, axis=1)

def uniq_rate(series):
    return len(set(series)) / len(series)

def mode_rate(series):
    a = series.values
    mode_fst = stats.mode(a)
    mode_snd = stats.mode(a[a != mode_fst.mode[0]])
    try:
        return mode_fst.count[0] / mode_snd.count[0]
    except IndexError:
        return 0

def remove_high_correlation_var(df, cutoff=0.90):
    def sort_by_mean(corr):
        copy = corr.copy()
        np.fill_diagonal(copy.values, np.nan)
        order = copy.mean(skipna=True).sort_values(ascending=False).index
        return corr.loc[order, order]

    corr = sort_by_mean(df.corr().abs())
    np.fill_diagonal(corr.values, np.nan)

    drop_flag = OrderedDict((col, False) for col in corr.columns)

    for i, idx in enumerate(corr.index[:-1]):
        if not any(corr[~np.isnan(corr)] > cutoff):
            break
        if drop_flag[idx]:
            continue
        for j, col in enumerate(corr.columns[(i + 1):-1]):
            if not all([drop_flag[idx], drop_flag[col]]) and corr.loc[idx, col] > cutoff:
                if np.nanmean(corr[idx]) > np.nanmean(corr.drop(col)):
                    drop_flag[idx] = True
                    corr.loc[idx, :] = np.nan
                    corr.loc[:, idx] = np.nan
                else:
                    drop_flag[col] = True
                    corr.loc[col, :] = np.nan
                    corr.loc[:, col] = np.nan

    dropped_columns = [col for col, flg in drop_flag.items() if flg]
    return df.drop(dropped_columns, axis=1)

def main():
    sns.set(style="ticks")

    action = read_action_daily('game-01', date(2013, 10, 31), date(2013, 10, 31))
    kmeans0 = KMeans(n_clusters=3).fit(action['A47'].values.reshape(-1, 1))

    group = pd.DataFrame(kmeans0.labels_, columns=['label']).groupby('label')
    print(group['label'].count())
    # plt.plot(range(len(action['user_id'])), sorted(action['A48'], reverse=True))
    # plt.show()

    action = a0 = action.drop(['log_date', 'app_name', 'user_id', 'A1'], axis=1)
    action = a1 = action[kmeans0.labels_ >= 1]
    action = a2 = remove_near_zero_var(action)
    action = a3 = remove_high_correlation_var(action, cutoff=0.7)

    X = np.array([StandardScaler().fit_transform(feature.reshape(-1, 1)).flatten()
                  for feature in action.values.T]).T
    pc = PCA(n_components='mle', svd_solver='full').fit_transform(X)

    kmeans = KMeans(n_clusters=5).fit(pc[:, 0].reshape(-1, 1))
    action['cluster'] = kmeans.labels_
    center = action.groupby('cluster').apply(lambda g: g.mean()).drop(['cluster'], axis=1)
    center = remove_high_correlation_var(center)
    X = np.array([StandardScaler().fit_transform(feature.reshape(-1, 1)).flatten()
                  for feature in center.values.T]).T
    center = pd.DataFrame(X, columns=center.columns)
    center.to_csv('./section8-center-mime.csv', index=False)
    ipy.embed()

if __name__ == '__main__':
    main()
