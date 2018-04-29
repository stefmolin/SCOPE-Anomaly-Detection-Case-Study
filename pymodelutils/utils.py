from sklearn.metrics import classification_report, zero_one_loss
from sklearn.metrics import confusion_matrix
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import re
import datetime

def get_linear_weights(length):
    return [(length - i)/length for i in range(length)]

def get_exponential_weights(length):
    return [np.exp(i - length) for i in range(length)][::-1]

def weight_data(df, method=['linear', 'exponential']):
    method = method.lower()
    if method == 'linear':
        weighting_function = get_linear_weights
    elif method == 'exponential':
        weighting_function = get_exponential_weights
    else:
        raise ValueError("Method can either be 'linear' or 'exponential'")

    diff_cols = sorted([col for col in df if col.startswith('time_delta_diff')])
    value_cols = sorted([col for col in df if col.startswith('time_delta') and col not in diff_cols])
    
    value_weights = weighting_function(len(value_cols))
    diff_weights = weighting_function(len(diff_cols))
    
    for col, weight in zip(value_cols + diff_cols, value_weights + diff_weights):
        df.loc[:,col] = df[col].multiply(weight)
    
def prep_dates(df):
    df['run_date'] = df['run_date'].astype('datetime64[ns]')
    df['day'] = df['day'].astype('datetime64[ns]')
    df['time_delta'] = df['run_date'] - df['day']
    df['time_delta'] = df['time_delta'].astype(datetime.timedelta).map(lambda x: np.nan if pd.isnull(x) else x.days)

def one_hot_encode(df, cols):
    return pd.get_dummies(df, columns=cols)

def diff_values(df):
    '''Calculate 1 period differences where period 1 is the most recent'''
    time_cols = [col for col in df.columns if 'time_delta' in col]
    diff_cols = ['time_delta_diff_' + ('' if x >= 10 else '0') + str(x) for x in range(1, len(time_cols) + 1)]
    df[diff_cols[::-1]] = df[time_cols[::-1]].diff(axis=1)
    
    # drop the column that has no difference
    df.drop(columns=diff_cols[-1], inplace=True)

def reshape_df(df, indices):
    '''Put the time_delta column as separate columns for each value.'''
    df = df.pivot_table(values='value',
                        index=indices,
                        columns='time_delta',
                        aggfunc=np.sum).reset_index()
    df.columns.name = None
    return df

def prefix_columns(df):
    df.columns = ['time_delta_' + ('' if col >= 10 else '0') + str(col) 
                  if re.search('\d', str(col)) else col for col in df.columns]
    
def clean_regions(df):
    df['isCountry'] = np.where(pd.isnull(df['country']), 0., 1.)
    df['isRegion'] = np.where(pd.isnull(df['subregion']), 1., 0.)
    df['isSubregion'] = np.where(df['isCountry'] + df['isRegion'] == 0, 1., 0.)
    df = df.drop(columns=['country', 'subregion', 'region'])
    return df

def add_summary_stats(df, take_log=True):
    value_cols = [col for col in df.columns if col.startswith('time_delta') and 'diff' not in col]
    df['mode'] = df[value_cols].mode(1).mean(1)
    df['var'] = df[value_cols].var(1)
    df['min'] = df[value_cols].min(1)
    df['max'] = df[value_cols].max(1)
    df['mean'] = df[value_cols].mean(1)
    
    stat_cols = ['mode', 'var', 'min', 'max', 'mean']
    
    if take_log:
        for col in stat_cols:
            df[col] = np.sign(df[col]) * df[col].abs().apply(np.log)
    else:
        kpi_cols = [col for col in df.columns if 'kpi' in col]
        if len(kpi_cols) > 0:
            for stat in stat_cols:
                for kpi_col in kpi_cols:
                    metric_min = df[df[kpi_col] == 1][stat].min()
                    metric_max = df[df[kpi_col] == 1][stat].max()
                    df.loc[df[kpi_col] == 1, stat] = (df[df[kpi_col] == 1][stat] - metric_min) / (metric_max - metric_min)
        else:
            df[stat_cols] = df[stat_cols].sub(df[stat_cols].min(axis=0), axis=1)\
                                .div(df[stat_cols].max(axis=0).sub(df[stat_cols].min(axis=0)), axis=1)

# normalize the data for the model here
def scale_data(df, method=['standardize', 'min_max', 'percent_of_mean']):
    """Scale data for use in model.
    
    Arguments:
        df        dataframe to be scaled
        method    Way to perform the scaling where
                    - standardize = subtract mean and divide by the variance
                    - min_max = subtract the min and divide by the difference between the min and the max
                    - percent_of_mean = divide by the mean
    """
    # replace anything that might be NaN with 0 for correct calculations
    df.fillna(0, inplace=True)
    diff_cols = [col for col in df.columns if col.startswith('time_delta_diff')]
    value_cols = [col for col in df.columns if col.startswith('time_delta') and col not in diff_cols]
    method = method.lower()
    if method == 'standardize':
        values = df[value_cols].sub(df[value_cols].mean(axis=1), axis=0)\
                    .div(df[value_cols].std(axis=1), axis=0)
        differences = df[diff_cols].sub(df[diff_cols].mean(axis=1), axis=0)\
                        .div(df[diff_cols].std(axis=1), axis=0)
    elif method == 'min_max':
        values = df[value_cols].sub(df[value_cols].min(axis=1), axis=0)\
                    .div(df[value_cols].max(axis=1).sub(df[value_cols].min(axis=1)), axis=0)
        differences = df[diff_cols].sub(df[diff_cols].min(axis=1), axis=0)\
                        .div(df[diff_cols].max(axis=1).sub(df[diff_cols].min(axis=1)), axis=0)
    elif method == 'percent_of_mean':
        values = df[value_cols].div(df[value_cols].mean(axis=1), axis=0)
        differences = df[diff_cols].div(df[diff_cols].mean(axis=1), axis=0)
    else:
        raise ValueError("'method' must be one of: 'standardize', 'min_max', 'percent_of_mean'")
    df[value_cols] = values
    df[diff_cols] = differences
    # some series will have a mean of 0 in which case all the time_delta columns will become NaN, so let's change those to 0
    df.fillna(0, inplace=True)
    return df

def data_prep_pipeline(df, indices, cols, scaling_method, weighting_type=None, summary_stats=False):
    reshaped = reshape_df(df, indices)
    encoded_reshaped = one_hot_encode(reshaped, cols)
    prefix_columns(encoded_reshaped)
    diff_values(encoded_reshaped)
    if summary_stats:
        add_summary_stats(encoded_reshaped)
    if isinstance(scaling_method, list) and len(scaling_method) > 0:
        final_form = {method:scale_data(encoded_reshaped, method) for method in scaling_method}
    else:
        final_form = {scaling_method:scale_data(encoded_reshaped, scaling_method)}
    if not weighting_type:
        return final_form
    else:
        for key, df in final_form.items():
            weight_data(df, weighting_type.lower())
    return final_form

def borderline_case_analysis(threshold, predictions, test_data, scaled_data, raw_data, data_type=['AS', 'TS', 'RexT']):
    data_type = data_type.upper()
    
    # separate alerts and non-alerts
    not_alerts = scaled_data.index[scaled_data.is_alert == 0].tolist()
    alerts = scaled_data.index[scaled_data.is_alert == 1].tolist()
    
    # determine what the model will and won't flag
    model_does_flag = predictions[:,1] >= threshold
    model_doesnt_flag = predictions[:,1] < threshold
    
    # get indices of false positives (sorted by probability of being an alert -- least to most likely)
    false_positive_likelihood = [i for i in np.argsort(predictions[:,1]) 
                                 if test_data.iloc[i].name in not_alerts and model_does_flag[i]]
    
    # get the indices of the false negatives (sorted by probability of being an alert -- most to least likely)
    false_negative_likelihood = [i for i in np.argsort(predictions[:,1])[::-1] 
                                 if test_data.iloc[i].name in alerts and model_doesnt_flag[i]]
    
    # isolate top 10 for each group
    top_10_false_positive = false_positive_likelihood[:10] # the least likely to be alerts but flagged anyway
    top_10_false_negative = false_negative_likelihood[:10] # the most likely to be alerts of those not flagged
    
    # create empty dataframe to hold all the data for the facet grid
    df = pd.DataFrame()
    
    for fp in top_10_false_positive:
        # get the name of the series in the other data
        index_name = test_data.iloc[fp].name
        
        # use the scaled data to determine the KPI
        if data_type == 'AS':
            kpi = scaled_data.iloc[index_name][[col for col in scaled_data.iloc[index_name].index 
                                                if 'kpi' in col]].astype(int).idxmax().split('kpi_')[1]
            # find the KPI evolution in the raw data
            time_series = raw_data.loc[(raw_data.series == scaled_data.iloc[index_name][0]) &
                                       (raw_data.run_date == scaled_data.iloc[index_name][1]) & 
                                       (raw_data.kpi == kpi)]
        elif data_type == 'TS':
            kpi = 'tag_events'
            time_series = raw_data.loc[(raw_data.series == scaled_data.iloc[index_name][0]) &
                                       (raw_data.run_date == scaled_data.iloc[index_name][1])]
        elif data_type == 'REXT':
            kpi = 'territory_rext'
            series = scaled_data.iloc[index_name][[col for col in scaled_data.iloc[index_name].index 
                                                if 'series' in col]].astype(int).idxmax().split('series_')[1]
            # find the KPI evolution in the raw data
            time_series = raw_data.loc[(raw_data.series == series) &
                                       (raw_data.run_date == scaled_data.iloc[index_name][3])]
        
        
        # add a column for the prediction value for later use
        time_series['alert_probability'] = predictions[:,1][fp] 
        time_series['result'] = 'FP'
        
        # collect all the data in one dataframe
        df = df.append(time_series)
    
    for fn in top_10_false_negative:
        # get the name of the series in the other data
        index_name = test_data.iloc[fn].name
        
        # use the scaled data to determine the KPI
        if data_type == 'AS':
            kpi = scaled_data.iloc[index_name][[col for col in scaled_data.iloc[index_name].index 
                                                if 'kpi' in col]].astype(int).idxmax().split('kpi_')[1]
            # find the KPI evolution in the raw data
            time_series = raw_data.loc[(raw_data.series == scaled_data.iloc[index_name][0]) &
                                       (raw_data.run_date == scaled_data.iloc[index_name][1]) & 
                                       (raw_data.kpi == kpi)]
        elif data_type == 'TS':
            kpi = 'tag_events'
            # find the KPI evolution in the raw data
            time_series = raw_data.loc[(raw_data.series == scaled_data.iloc[index_name][0]) &
                                       (raw_data.run_date == scaled_data.iloc[index_name][1])]
        elif data_type == 'REXT':
            kpi = 'territory_rext'
            series = scaled_data.iloc[index_name][[col for col in scaled_data.iloc[index_name].index 
                                                if 'series' in col]].astype(int).idxmax().split('series_')[1]
            # find the KPI evolution in the raw data
            time_series = raw_data.loc[(raw_data.series == series) &
                                       (raw_data.run_date == scaled_data.iloc[index_name][3])]
        
        # add a column for the prediction value for later use
        time_series['alert_probability'] = predictions[:,1][fn] 
        time_series['result'] = 'FN'
        
        # collect all the data in one dataframe
        df = df.append(time_series)
    return df
    
def mismatched_analysis(borderline_df, data_type=['AS','TS','RexT']):
    data_type = data_type.upper()
    if data_type == 'AS':
        borderline_df = borderline_df[['result', 'is_campaign', 'kpi', 'series', 'run_date']].drop_duplicates()
        return pd.crosstab(index=borderline_df['result'], columns=[borderline_df['is_campaign'], borderline_df['kpi']])
    elif data_type == 'TS':
        borderline_df = borderline_df[['result', 'kpi', 'series', 'run_date']].drop_duplicates()
        return pd.crosstab(index=borderline_df['result'], columns=[borderline_df['kpi']])
    elif data_type == 'REXT':
        borderline_df = borderline_df[['result', 'kpi', 'series', 'run_date']].drop_duplicates()
        return pd.crosstab(index=borderline_df['result'], columns=[borderline_df['series']])
    else:
        raise ValueError("Only 'AS', 'TS', and 'RexT' are supported values for data_type")
        
def mismatched_graphs(df, data_type=['AS', 'TS', 'RexT']):
    graph_data = df.copy().reset_index(drop=True)
    graph_data['feature_day'] = [i % 25 + 1 for i in range(0, graph_data.shape[0])]
    
    data_type = data_type.upper()
    if data_type == 'AS': 
        for result in graph_data.result.unique():
            print('---------------------------------------\n| Plotting the ' + result + ' data.')
            plot_data = graph_data[graph_data.result == result]
            for kpi in plot_data.kpi.unique():
                print('---------------------------------------\n| ' + result + ' on ' + 'KPI = ' + kpi + '\n---------------------------------------')
                sns.lmplot(x='feature_day', y='value', data=plot_data[plot_data.kpi == kpi], 
                           hue='is_campaign', fit_reg=False, col='alert_probability', 
                           col_wrap=3, sharex=True, sharey=True, markers='>')
                plt.show()
    elif data_type == 'TS':
        for result in graph_data.result.unique():
            print('---------------------------------------\n| Plotting the ' + result + ' data.')
            plot_data = graph_data[graph_data.result == result]
            for kpi in plot_data.kpi.unique():
                print('---------------------------------------\n| ' + result + ' on ' + 'KPI = ' + kpi + '\n---------------------------------------')
                sns.lmplot(x='feature_day', y='value', data=plot_data[plot_data.kpi == kpi],  fit_reg=False, col='alert_probability', 
                           col_wrap=3, sharex=True, sharey=True, markers='>')
                plt.show()
    elif data_type == 'REXT':
        for result in graph_data.result.unique():
            print('---------------------------------------\n| Plotting the ' + result + ' data.')
            plot_data = graph_data[graph_data.result == result]
            for kpi in plot_data.kpi.unique():
                print('---------------------------------------\n| ' + result + ' on ' + 'KPI = ' + kpi + '\n---------------------------------------')
                sns.lmplot(x='feature_day', y='value', data=plot_data[plot_data.kpi == kpi],  fit_reg=False, col='alert_probability', 
                           hue='series', col_wrap=3, sharex=True, sharey=True, markers='>')
                plt.show()
    else:
        raise ValueError("Only 'AS', 'TS', and 'RexT' are supported values for data_type")

# ROC Curve
def model_roc_curves(roc_data_dict, auc_dict, method_name):
    plt.plot([0, 1], [0, 1], color='navy', lw=2, linestyle='--')
    plt.plot(roc_data_dict['AS'][0], roc_data_dict['AS'][1], # needs fpr, tpr
             color='red', lw=2, label='AS ROC curve (area = %0.2f)' % auc_dict['AS'])
    plt.plot(roc_data_dict['TS'][0], roc_data_dict['TS'][1],
             color='blue', lw=2, label='TS ROC curve (area = %0.2f)' % auc_dict['TS'])
    plt.plot(roc_data_dict['RexT'][0], roc_data_dict['RexT'][1],
             color='green', lw=2, label='RexT ROC curve (area = %0.2f)' % auc_dict['RexT'])
    plt.xlim([0.0, 1.0])
    plt.ylim([0.0, 1.05])
    plt.xlabel('False Positive Rate')
    plt.ylabel('True Positive Rate')
    plt.title(method_name + ' ROC Curve')
    plt.legend(loc="lower right")
    plt.show()
    
# see also: http://scikit-learn.org/stable/auto_examples/model_selection/plot_roc.html#sphx-glr-auto-examples-model-selection-plot-roc-py


def confusion_matrix_visual(y_true, y_pred, series_name, **kwargs):
    mat = confusion_matrix(y_true, y_pred)
    print('TP to FP ratio: ' + str(round(mat[1,1] / mat[0,1], 2)))
    sns.heatmap(mat.T, square=True, annot=True, fmt='d', cbar=True, cmap=plt.cm.Blues, **kwargs)
    plt.xlabel('Metis Feedback')
    plt.ylabel('Model Prediction')
    classes = ['FALSE', 'TRUE']
    tick_marks = np.arange(len(classes)) + 0.5
    plt.xticks(tick_marks, classes)
    plt.yticks(tick_marks, classes, rotation=0)
    plt.title(series_name + ' Confusion Matrix -- TRUE means alert')
    plt.show()


def classification_report_all(y_test_dict, y_pred_dict):
    print("AS results")
    print(classification_report(y_test_dict['AS'], y_pred_dict['AS'], target_names=['False', 'True']))
    print('Percent misclassified: ' + \
      str(round(zero_one_loss(y_test_dict['AS'], y_pred_dict['AS'])*100, 2)) + '%')
    print('Count misclassified: ' + \
      str(zero_one_loss(y_test_dict['AS'], y_pred_dict['AS'], normalize=False)))
    print("------------------------------------------------------\nTS results")
    print(classification_report(y_test_dict['TS'], y_pred_dict['TS'], target_names=['False', 'True']))
    print('Percent misclassified: ' + \
      str(round(zero_one_loss(y_test_dict['TS'], y_pred_dict['TS'])*100, 2)) + '%')
    print('Count misclassified: ' + \
      str(zero_one_loss(y_test_dict['TS'], y_pred_dict['TS'], normalize=False)))
    print("------------------------------------------------------\nRexT results")
    print(classification_report(y_test_dict['RexT'], y_pred_dict['RexT'], 
                                target_names=['False', 'True']))
    print('Percent misclassified: ' + \
      str(round(zero_one_loss(y_test_dict['RexT'], y_pred_dict['RexT'])*100, 2)) + '%')
    print('Count misclassified: ' + \
      str(zero_one_loss(y_test_dict['RexT'], y_pred_dict['RexT'], normalize=False)))
    
def threshold_results(AS_predictions, TS_predictions, RexT_predictions):
    for threshold in [i/10 for i in list(range(11))]:
        print(str(int(threshold*100)) + '% threshold')
        print('AS alert count: ' + \
              str((AS_predictions[:,1] >= threshold).sum()))
        print('TS alert count: ' + \
              str((TS_predictions[:,1] >= threshold).sum()))
        print('RexT alert count: ' + \
              str((RexT_predictions[:,1] >= threshold).sum()))
        print('---------------------------------------------------------')