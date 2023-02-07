import os
import zipfile
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import Select
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException
import shutil
import time
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.action_chains import ActionChains
import pandas as pd
import numpy as np
import requests
from bs4 import BeautifulSoup
from soup2dict import convert
import datetime
import re



def get_chromedriver(chromedriver_path=None, use_proxy=False, user_agent=None,
                    PROXY_HOST=None, PROXY_PORT=None, PROXY_USER=None, PROXY_PASS=None, download_folder=None):

    manifest_json = """
    {
        "version": "1.0.0",
        "manifest_version": 2,
        "name": "Chrome Proxy",
        "permissions": [
            "proxy",
            "tabs",
            "unlimitedStorage",
            "storage",
            "<all_urls>",
            "webRequest",
            "webRequestBlocking"
        ],
        "background": {
            "scripts": ["background.js"]
        },
        "minimum_chrome_version":"22.0.0"
    }
    """

    background_js = """
    var config = {
            mode: "fixed_servers",
            rules: {
            singleProxy: {
                scheme: "http",
                host: "%s",
                port: parseInt(%s)
            },
            bypassList: ["localhost"]
            }
        };

    chrome.proxy.settings.set({value: config, scope: "regular"}, function() {});

    function callbackFn(details) {
        return {
            authCredentials: {
                username: "%s",
                password: "%s"
            }
        };
    }

    chrome.webRequest.onAuthRequired.addListener(
                callbackFn,
                {urls: ["<all_urls>"]},
                ['blocking']
    );
    """ % (PROXY_HOST, PROXY_PORT, PROXY_USER, PROXY_PASS)

    chrome_options = webdriver.ChromeOptions()
    # allow multiple download
    prefs_experim = {'profile.default_content_setting_values.automatic_downloads': 1}
    
    if use_proxy:
        pluginfile = 'proxy_auth_plugin.zip'

        with zipfile.ZipFile(pluginfile, 'w') as zp:
            zp.writestr("manifest.json", manifest_json)
            zp.writestr("background.js", background_js)
        chrome_options.add_extension(pluginfile)
    if user_agent:
        chrome_options.add_argument('--user-agent=%s' % user_agent)
    if download_folder:
        prefs_experim["download.default_directory"] = download_folder

    chrome_options.add_experimental_option("prefs", prefs_experim)
    driver = webdriver.Chrome(
        executable_path=chromedriver_path,
        chrome_options=chrome_options)
    return driver


def download_from_drive_dropbox(chromedriver_path='', download_url='', download_folder='', temp_folder='', pdf_name='',
                                move_folder='', source=''):
    
    # create folder
    if os.path.exists(temp_folder):
        shutil.rmtree(temp_folder)

    os.makedirs(temp_folder)
    
    # download file
    driver = get_chromedriver(chromedriver_path = chromedriver_path, download_folder=download_folder)
    driver.get(download_url)
    if source == 'drive':
        download_button_xpath = ["/html/body/div[3]/div[4]/div/div[3]/div[2]/div[2]/div[3]"]
    elif source == 'dropbox':
        download_button_xpath = ["/html/body/div[3]/div/div/div/div[2]/div/div/div/div[1]/header/div[1]/div[1]/button/span",
                                 "/html/body/div[1]/div/div/div/div[2]/div/div/div/div[1]/header/div[1]/div[1]/span/button/span",
                                 "/html/body/div[3]/div/div/div/div[2]/div/div/div/div[1]/header/div[1]/div/span/button"]                                
        time.sleep(2)
        ActionChains(driver).send_keys(Keys.ESCAPE).perform()
    else:
        raise ValueError('Please provide "soure", can be "drive" or "dropbox"')
    page_error = "page not available"
    try:
        time.sleep(1.5)
        driver.find_element("xpath", download_button_xpath[0]).click()
        page_error = ""
    except:
        pass
    try:
        time.sleep(1.5)
        driver.find_element("xpath", download_button_xpath[1]).click()
        page_error = ""
    except:
        pass
    try:
        time.sleep(1.5)
        driver.find_element("xpath", download_button_xpath[2]).click()
        page_error = ""
    except:
        pass
    if page_error != "":
        return page_error
    
    # wait for download
    end_time = time.time() + 30
    while not any(x.endswith('.pdf') for x in os.listdir(temp_folder)):
        time.sleep(1)
        if time.time() > end_time:
            return "out of time"
    time.sleep(1)
    driver.close()
    
    # rename and move
    file_list = os.listdir(temp_folder)
    if len(file_list) > 0:
        os.rename(os.path.join(temp_folder, file_list[0]), os.path.join(temp_folder, pdf_name))
        shutil.move(os.path.join(temp_folder, pdf_name), os.path.join(move_folder, pdf_name))
        try:
            shutil.rmtree(temp_folder)
        except:
            pass
        return "ok"
    else:
        try:
            shutil.rmtree(temp_folder)
        except:
            pass
        return "file not found"


def adjust_date(x):
    # cut Month name to 3 char only
    x=x.split(' ')
    if len(x) > 1:
        x[1]=x[1][:3]
        x=' '.join(x)
    else:
        x=x[0]
        
    return x


def get_social_series(url='', tot_series=1, chromedriver_path=''):
    
    # https://levelup.gitconnected.com/trickycases-6-scrape-highcharts-plots-a6b3fc233fe6
    
    driver = get_chromedriver(chromedriver_path=chromedriver_path)
    driver.get(url)
    series_dict={}
    try:
        WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.CLASS_NAME, "highcharts-container ")))

        for i in range(tot_series):            
            cmd='return Highcharts.charts[0].series['+str(i)+']'
            series_name=driver.execute_script(cmd+'.name')
            dates = driver.execute_script(cmd+'.data.map(x => x.series).map(x => x.xData)[0].map(x => new Date(x).toISOString())')
            values = driver.execute_script(cmd+'.data.map(x => x.series).map(x => x.yData)[0]')

            series_dict[series_name]=pd.DataFrame({'Date': dates, 'Users': values})
    except:
        pass

    driver.close()

    return series_dict


def scrape_info_icomarks(url='', chromedriver_path=''):
    
    add_row=pd.DataFrame()
    
    #### request page
    page = requests.get(url)
    soup = BeautifulSoup(page.content, 'html.parser')


    #### page screenshot date
    add_row['url']=[url]
    add_row['PageScreenshot']=[soup.findAll(text = re.compile('Last screenshot'))]


    #### extract rating

    tag = soup.find_all('div', class_="ico-rating-content", recursive=True)
    conv_dict = convert(tag)
    if len(conv_dict) > 0:
        # overall rating
        ind=np.where([x['@class'] == ['ico-rating-overall'] for x in conv_dict['div'][0]['div']])[0][0]
        value=conv_dict['div'][0]['div'][ind]['#text']   
        add_row['Rating_TOTAL_SCORE']=[value]
        # rating component
        for t in conv_dict['div'][0]['div'][2]['div']:
            if '@class' in t.keys():
                if t['@class'] == ['ico-rating__item']:
                    ind=np.where([x['@class'] == ['ico-rating__title'] for x in t['div']])[0][0]
                    name=t['div'][ind]['#text']
                    name=re.sub('ICO |STO |IEO ', '', name)
                    ind=np.where([x['@class'] == ['ico-rating__circle'] for x in t['div']])[0][0]
                    value=t['div'][ind]['#text']
                    add_row['Rating_'+name.replace(' ', '_')]=[value]



    #### extract "Detail" tab blocks

    tag = soup.find_all('div', class_="icoinfo-block", recursive=True)
    conv_dict = convert(tag)

    block_log={}
    block_df=pd.DataFrame()
    for t in conv_dict['div']:

        # block name
        ind=np.where([x['@class'] == ['icoinfo-block__title'] for x in t['div']])[0][0]
        block_name=t['div'][ind]['#text']

        # extract block contents
        try:
            ind=np.where([x['@class'] == ['icoinfo-block-content'] for x in t['div']])[0][0]
            block_log[block_name]=t['div'][ind]['#text']   # save log of displayed items
            block_dict=t['div'][ind]['div']
        except:   # means tab of social media
            block_dict=t['div']

        block_t=pd.DataFrame()
        for t1 in block_dict:
            if t1['@class'] == ['icoinfo-block__item']:
                item_name=t1['span'][0]['#text'].replace(':', '')
                item_url=None
                if 'a' in t1.keys():
                    if '@href' in t1['a'][0]:
                        item_url=t1['a'][0]['@href']                
                    item_display=t1['a'][0]['navigablestring']
                elif 'i' in t1.keys():
                    if 'navigablestring' in t1['i'][0]:
                        item_display=t1['i'][0]['navigablestring']
                else:
                    try:
                        item_display=t1['navigablestring']
                    except:
                        item_display=None
                if type(item_display) == list:
                    if len(item_display) > 1:
                        item_display='; '.join(item_display)
                    else:
                        item_display=item_display[0]
                block_t=pd.concat([block_t, pd.DataFrame({'BlockName': block_name,
                                                         'Item': item_name,
                                                         'ItemUrl': item_url,
                                                         'ItemValue': item_display}, index=[0])])

        block_df=pd.concat([block_df, block_t])
    add_row['InfoBlock']=[block_df]


    #### Get team size and members

    tag = soup.find_all('a', href=True, recursive=True)
    conv_dict = convert(tag)
    ind=np.where([x['@href'] == '#team' for x in conv_dict['a']])[0][0]
    team_size=int(conv_dict['a'][ind]['#text'].replace('Team (', '').replace(')', ''))
    # check if Advisors
    advisor_size=int(soup.findAll(text = re.compile('Advisors \('))[0].replace('Advisors (', '').replace(')', ''))
    loop_max = 2 if advisor_size != 0 else 1
    # extract Team and Advisors
    tag = soup.find_all('div', class_='company-team', recursive=True)
    conv_dict = convert(tag)
    team_df=pd.DataFrame()
    for team_ind in range(loop_max):    # 0 = Team  1 = Advisor
        team_lab = 'Team' if team_ind == 0 else 'Advisor'
        for t in conv_dict['div'][team_ind]['div']:
            person_name=t['div'][0]['#text']
            person_role=t['div'][1]['#text']
            person_extra=None
            if t['div'][2]['@class'] == ['company-team__post']:
                person_extra=t['div'][2]['#text']
            if ['company-team__links'] in [x['@class'] for x in t['div']]:
                ind=np.where([x['@class'] == ['company-team__links'] for x in t['div']])[0][0]
                person_link=[x['@href'] for x in t['div'][ind]['a']]
            else:
                person_link=None
            team_df=pd.concat([team_df, pd.DataFrame({'Member': team_lab,
                                                      'Name': person_name,
                                                      'Role': person_role,
                                                      'Extra': person_extra,
                                                      'Links': [person_link]}, index=[0])])
    if team_size != team_df[team_df['Member']=='Team'].shape[0]:
        print(f"- {url} mismatch between 'TeamSize' and extracted team members")
    if advisor_size != team_df[team_df['Member']=='Advisor'].shape[0]:
        print(f"- {url} mismatch between 'AdvisorSize' and extracted advisor members")
    add_row['TeamSize']=team_size
    add_row['AdvisorSize']=advisor_size
    add_row['TeamBlock']=[team_df]


    #### Get Social Rating and users timeseries

    tag = soup.find_all('div', class_='companyTab companyTab_social', recursive=True)
    conv_dict = convert(tag)

    if len(conv_dict) > 0:
        social_df=pd.DataFrame()
        for t in conv_dict['div'][0]['div'][0]['div']:
            if t['@class'] == ['social-item']:
                social_name=t['div'][0]['div'][0]['#text']
                total_user=int(t['div'][1]['div'][2]['#text'].replace(',', ''))
                rating=t['div'][1]['div'][3]['#text']
                social_df=pd.concat([social_df, pd.DataFrame({'Social': social_name,
                                                             'Users': total_user,
                                                             'Rating': rating}, index=[0])])
        # download chart data
        series_dict=get_social_series(url=url, tot_series=social_df.shape[0], chromedriver_path=chromedriver_path)
        series_status = 'DOWNLOADED' if len(series_dict) != 0 else 'DOWNLOAD_ERROR'

        add_row['SocialWithRating']=social_df.shape[0]
        add_row['SocialSeriesStatus']=series_status
        add_row['SocialBlock']=[[{'stats': social_df, 'timeseries': series_dict}]]
        
    return add_row