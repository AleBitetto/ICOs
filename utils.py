import os
import zipfile
# from selenium import webdriver
from seleniumwire import webdriver
from seleniumwire.utils import decode
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support.ui import Select
from selenium.webdriver.chrome.service import Service
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException
import shutil
import time
from timeit import default_timer as timer
import pandas as pd
import numpy as np
import requests
import joblib
from bs4 import BeautifulSoup
from bs4 import Tag, NavigableString
from soup2dict import convert
import datetime
import re
import logging
from pdf2image import convert_from_path
import pytesseract
from PIL import Image
from tika import parser
from thefuzz import process
import json
from urllib.parse import urljoin
import torch
from transformers import BertTokenizer, BertForSequenceClassification, MPNetPreTrainedModel, MPNetModel, AutoTokenizer, AutoModelForSequenceClassification
from collections import OrderedDict
from termcolor import colored
from totaltimeout import Timeout
from cleantext import clean
from Crypto.Hash import MD5
from Crypto.Util.Padding import unpad
from Crypto.Cipher import AES
import base64
from requests import Request, Session
from requests.exceptions import ConnectionError, Timeout, TooManyRedirects
from IPython.display import display_html



def get_chromedriver(chromedriver_path=None, use_proxy=False, user_agent=None,
                    PROXY_HOST=None, PROXY_PORT=None, PROXY_USER=None, PROXY_PASS=None, download_folder=None,
                    desired_capabilities=None, return_options_only=False):

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

    if return_options_only:
        return chrome_options
    else:
        driver = webdriver.Chrome(
            service=Service(chromedriver_path), # executable_path=chromedriver_path,
            options=chrome_options,
            desired_capabilities=desired_capabilities)
        
        return driver


def pdf_to_text(file_path='', tesseract_path='', lang='eng', timeout=60, return_pages=False):
    
    headers = {
            "X-Tika-OCRLanguage": lang,
#             "X-Tika-OCRTimeout": str(timeout)
        }
    
    # open pdf
    parsed_pdf = parser.from_file(file_path, requestOptions={'headers': headers, 'timeout': 300})

    # extract text and metadata
    txt = parsed_pdf['content']
    meta = parsed_pdf['metadata']

    # if txt is None try to parse pdf as image and extract text with OCR
    # https://softhints.com/python-extract-text-from-image-or-pdf/
    # https://towardsdatascience.com/extracting-text-from-scanned-pdf-using-pytesseract-open-cv-cd670ee38052
    # install tesseract https://linuxhint.com/install-tesseract-windows/
    if txt is None:
        try:
            pytesseract.pytesseract.tesseract_cmd=tesseract_path

            # convert pages to images
            pages = convert_from_path(file_path, 350)
            
            if return_pages:
                page_text=[]
                for pag in pages:
                    page_text.append(pytesseract.image_to_string(pag, lang = lang))
                return page_text

            # extract text
            txt=''
            for pag in pages:
                txt=txt+'\n'+pytesseract.image_to_string(pag, lang = lang)
        except:
            pass
    return txt, meta, parsed_pdf


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
        download_button_xpath = ["/html/body/div[3]/div[4]/div/div[3]/div[2]/div[2]/div[3]",
                                 "/html/body/div[2]/div[4]/div/div[3]/div[2]/div[2]/div[3]"]
    elif source == 'dropbox':
        download_button_xpath = ["/html/body/div[3]/div/div/div/div[2]/div/div/div/div[1]/header/div[1]/div[1]/button/span",
                                 "/html/body/div[1]/div/div/div/div[2]/div/div/div/div[1]/header/div[1]/div[1]/span/button/span",
                                 "/html/body/div[3]/div/div/div/div[2]/div/div/div/div[1]/header/div[1]/div/span/button"]                                
        time.sleep(2)
        ActionChains(driver).send_keys(Keys.ESCAPE).perform()
    else:
        raise ValueError('Please provide "source", can be "drive" or "dropbox"')
    page_error = "page not available"
    try:
        time.sleep(1.5)
        driver.find_element(By.CLASS_NAME, "action-bar-action-DOWNLOAD_ACTION").click()
        page_error = ""
    except:
        pass
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


def get_list_icomarks(soup, MAIN_PAGE):
    
    temp_list = pd.DataFrame()
    conv_dict = convert(soup)['div']
    for elem in conv_dict:

        n_views=None
        ver_email=None
        is_sto=None
        is_ieo=None
        status=None
        start_date=None
        end_date=None
        for x in elem['div']:
            if x['@class'][0] == 'icoListItem__info':
                sup = x['a'][0]['sup']
                n_views = [y['#text'] for y in sup if y['@class'][0] == "sup_views"][0]
                is_sto = int(any([True if y['@class'][0] == "sup_is_sto" else False for y in sup]))
                is_ieo = int(any([True if y['@class'][0] == "sup_is_ieo" else False for y in sup]))
                ver_email = int(any([True if y['@class'][0] == "sup_email_confirmed" else False for y in sup]))
                url = x['a'][0]['@href']
            if x['@class'][0] == 'icoListItem__raised':
                status = x['#text']#[v['#text'] for k, v in x['span'][0].items() if ]
            if x['@class'][0] == 'icoListItem__start':
                start_date = x['navigablestring'][0]
            if x['@class'][0] == 'icoListItem__end':
                end_date = x['navigablestring'][0]

        temp_list = pd.concat([temp_list,
                               pd.DataFrame({
                                   'url': urljoin(MAIN_PAGE, url),
                                   'NViews': int(n_views.replace(' Views', '').replace(',', '')),
                                   'VerifiedEmailDummy': ver_email,
                                   'IsSTODummy': is_sto,
                                   'IsIEODummy': is_ieo,
                                   'Status': status.replace('STATUS ', ''),
                                   'StartDate': start_date,
                                   'EndDate': end_date}, index = [0])
                              ])
        
    return temp_list
    
    
def get_icos_list_by_category(url, categ, CHROMEDRIVER_PATH, MAIN_PAGE, NEW_DOMAIN, split=False):
    
    # check if empty results
    page = requests.get(url)
    if 'No results for this search' in BeautifulSoup(page.content, 'html.parser').prettify():
        return None
    
    if not split:
        print('   - Scrolling down...', end ='')

    driver = get_chromedriver(chromedriver_path = CHROMEDRIVER_PATH)
    driver.get(url)
    
    # scroll down till "Show more" button disappear
    show_more_path = '/html/body/section/div[2]/div[2]/div[2]/a'
    
    try:
        while driver.find_element("xpath", show_more_path).is_displayed():

            driver.execute_script("arguments[0].scrollIntoView(true);", driver.find_element("xpath", show_more_path))
            driver.find_element("xpath", show_more_path).click()
            time.sleep(3)
        if not split:
            print('OK')
    except:
        if not split:
            print('SKIPPED')

    # get html
    if not split:
        print('   - Downloading html...', end='')
    soup = BeautifulSoup(driver.page_source, 'html.parser')
    if not split:
        print('OK')

    # extract information from web list
    if not split:
        print('   - Parsing info...', end='')
    tag = soup.find_all('div', class_="icoListContent", recursive=True)
    tag_list = []
    for t in tag[0]:
        if 'div class="newItems"' not in str(t):
            tag_list.append(t)
    # if show more, html structure changes
    nested_tags=soup.find_all('div', class_="newItems", recursive=True)
    if len(nested_tags) > 0:
        for x in soup.find_all('div', class_="newItems", recursive=True):
            tag_list.extend([y for y in x])

    temp_list = pd.DataFrame()
    for t in tag_list:

        if 'START' in str(t):

            conv_dict = convert(t)['div']
            for x in conv_dict:

                if x['@class'][0] == 'icoListItem__info':
                    sup = x['a'][0]['sup']
                    n_views = [y['#text'] for y in sup if y['@class'][0] == "sup_views"][0]
                    is_sto = int(any([True if y['@class'][0] == "sup_is_sto" else False for y in sup]))
                    is_ieo = int(any([True if y['@class'][0] == "sup_is_ieo" else False for y in sup]))
                    ver_email = int(any([True if y['@class'][0] == "sup_email_confirmed" else False for y in sup]))
                    url = x['a'][0]['@href']
                if x['@class'][0] == 'icoListItem__raised':
                    status = x['#text']#[v['#text'] for k, v in x['span'][0].items() if ]
                if x['@class'][0] == 'icoListItem__start':
                    start_date = x['navigablestring'][0]
                if x['@class'][0] == 'icoListItem__end':
                    end_date = x['navigablestring'][0]

            temp_list = pd.concat([temp_list,
                                   pd.DataFrame({
                                       'Category':categ,
                                       'url': urljoin(MAIN_PAGE.replace(".com", NEW_DOMAIN), url),
                                       'NViews': int(n_views.replace(' Views', '').replace(',', '')),
                                       'VerifiedEmailDummy': ver_email,
                                       'IsSTODummy': is_sto,
                                       'IsIEODummy': is_ieo,
                                       'Status': status.replace('STATUS ', ''),
                                       'StartDate': start_date,
                                       'EndDate': end_date}, index = [0])
                                  ])

    driver.close()
        
    return temp_list


def adjust_date(x):
    # cut Month name to 3 char only
    x=x.split(' ')
    if len(x) > 1:
        x[1]=x[1][:3]
        x=' '.join(x)
    else:
        x=x[0]
        
    return x


def eval_duration(x):
    
    duration=None
    if x['StartDate'] not in ['TBA', '']:
        try:
            max_date=pd.to_datetime(x['ListDownloadedOn'], format="%d/%m/%Y")
            start=pd.to_datetime(x['StartDate'])
            end=pd.to_datetime(x['EndDate']) if x['EndDate'] not in ['TBA', ''] else pd.to_datetime('1 Jan 2150')
            duration=min([end, max_date])-start
            if duration.days < 0:
                duration=-99
            else:
                duration=np.log10(duration.days + 1)   # avoid log(0)
        except:
            duration=-99
            print(f'#### wrong date on index {x.name}: {x.StartDate} or {x.EndDate}')
    
    return(duration)


def get_social_series(driver=None, tot_series=1):
    
    # https://levelup.gitconnected.com/trickycases-6-scrape-highcharts-plots-a6b3fc233fe6
    
    series_dict={}
    status=''
    try:
        WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.CLASS_NAME, "highcharts-container ")))
    except:
        status='DOWNLOAD_NOT_AVAILABLE'
    else:
        try:
            for i in range(tot_series):            
                cmd='return Highcharts.charts[0].series['+str(i)+']'
                series_name=driver.execute_script(cmd+'.name')
                dates = driver.execute_script(cmd+'.data.map(x => x.series).map(x => x.xData)[0].map(x => new Date(x).toISOString())')
                values = driver.execute_script(cmd+'.data.map(x => x.series).map(x => x.yData)[0]')

                series_dict[series_name]=pd.DataFrame({'Date': dates, 'Users': values})
        except:
            status='DOWNLOAD_ERROR'

    return status, series_dict


def get_price_series(driver=None):

    status=''
    count=0
    try:
        valueToClick = "All"
        button = driver.find_element('xpath',
                                     '//div[@class="companyGraph"]//div[@class="highcharts-container "]//*[name()="g" and '
                                     f'@class="highcharts-range-selector-group"]//*[name()="text" and text()="{valueToClick}"]')
    except:
        status='DOWNLOAD_NOT_AVAILABLE'
    else:
        try:
            button.click()

            status='DOWNLOADED'
            multiple_series=[]
            multiple_data=[]
            for request in driver.requests:
                if request.response:
                    if 'https://icomarks.ai/graph/prices?' in request.url:
                        body = decode(request.response.body, request.response.headers.get('Content-Encoding', 'identity'))
                        data=json.loads(body)
                        multiple_data.append(data)
                        multiple_series.append(int(data['total']))
                        count+=1
        except:
            status='DOWNLOAD_ERROR'

    series_df=None
    if count > 0:
        data=multiple_data[np.argmax(multiple_series)]
        for col in ['prices', 'market_cap', 'h24_vol']:
            df_t=pd.DataFrame(data[col], columns =['Date', col])
            df_t['Date']=pd.to_datetime(df_t['Date'], unit='ms')
            if series_df is not None:
                series_df=series_df.merge(df_t, on='Date', how='left')
            else:
                series_df=df_t
        series_df.columns=['Date', 'PriceUSD', 'MarketCap', 'Volume24H']
    if count > 1 and len(set(multiple_series)) > 1:
        status='DOWNLOADED_BUT_MULTIPLE_SERIES'
    if series_df is None:
        series_df=pd.DataFrame()
        
    return status, series_df


def scrape_info_icomarks(url='', URL_ROOT='', PRICE_API='', SOCIAL_API='', skip_social=False, skip_price=False):
# def scrape_info_icomarks(url='', chromedriver_path='', skip_social=False, skip_price=False):
    '''
    - skip_social: if True skip social users' timeseries download (takes time and uses WebDriver)
    - skip_price: if True skip market price timeseries download (takes time and uses WebDriver)
    
    Better to allow or deny both.
    '''

    add_row=pd.DataFrame()

    #### request page
    page = requests.get(url)
    soup = BeautifulSoup(page.content, 'html.parser')


    #### page screenshot date
    add_row['url']=[url]
    add_row['name']=soup.find("div", class_="company-h1").find("h1").get_text()
    try:
        tag = soup.find_all('div', class_="swimm-panel-bottom__links", recursive=True)
        conv_dict = convert(tag)
        add_row['PageScreenshot']=[conv_dict['div'][0]['#text']]
    except:
        pass


    #### get categories
    categ_list=[]
    try:
        tt=convert(soup.find("div", class_="company-category-links"))
        for el in tt['a']:
            categ_list.append(el['#text'])
    except:
        pass
    add_row['category']=[categ_list]


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

    try:
        tag = soup.find_all('a', href=True, recursive=True)
        conv_dict = convert(tag)
        ind=np.where([x['@href'] == '#team' for x in conv_dict['a']])[0][0]
        team_size=int(conv_dict['a'][ind]['#text'].replace('Team (', '').replace(')', ''))
        # check if Advisors
        advisor_size=int(soup.find_all(string = re.compile('Advisors \('))[0].replace('Advisors (', '').replace(')', ''))
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
    except:
        pass


    #### Get Social Rating and users timeseries

    tag = soup.find_all('div', class_='companyTab companyTab_social', recursive=True)
    conv_dict = convert(tag)

    if len(conv_dict) > 0:
        social_df=pd.DataFrame()
        try:
            for t in conv_dict['div'][0]['div'][0]['div']:
                if t['@class'] == ['social-item']:
                    social_name=t['div'][0]['div'][0]['#text']
                    total_user=int(t['div'][1]['div'][2]['#text'].replace(',', ''))
                    rating=t['div'][1]['div'][3]['#text']
                    social_df=pd.concat([social_df, pd.DataFrame({'Social': social_name,
                                                                 'Users': total_user,
                                                                 'Rating': rating}, index=[0])])
        except:
            pass

        # download chart data
        if not skip_social:
    #         driver = get_chromedriver(chromedriver_path=chromedriver_path)
    #         driver.get(url)
    #         series_status, series_dict=get_social_series(driver=driver, tot_series=social_df.shape[0])
    #         series_status = 'DOWNLOADED' if len(series_dict) != 0 else series_status
            series_dict={}
            try:
                social_url=SOCIAL_API + url.replace(URL_ROOT, '').strip("/")
                social_page = requests.get(social_url)
                data=social_page.json()
                for soc in data.keys():
                    df=pd.DataFrame(data[soc], columns=['Date', 'Users'])
                    df['Date']=pd.to_datetime(df['Date'], unit='ms')
                    series_dict[soc.capitalize()]=df
                series_status = 'DOWNLOADED'
            except:
                series_status='DOWNLOAD_ERROR'
        else:
            series_dict={}
            series_status='DOWNLOAD_SKIPPED'

        add_row['SocialWithRating']=social_df.shape[0]
        add_row['SocialSeriesStatus']=series_status
        add_row['SocialBlock']=[[{'stats': social_df, 'timeseries': series_dict}]]
    else:
        add_row['SocialSeriesStatus']='SOCIAL_TAB_MISSING'

        
    #### Get Milestones
    
    tag = soup.find('div', id='milestones', recursive=True)

    if tag:
        try:
            tt=convert(tag.find('div', class_="milestones"))
            milestone_df=pd.DataFrame()
            for el in tt['div']:
                date=''
                milestone=''
                for i, e in enumerate(el['div']):
                    if e['@class'] == ['milestones__date']:
                        date=e['#text']
                    if e['@class'] == ['milestones-content']:
                        milestone=e['#text']
                milestone_df=pd.concat([milestone_df, pd.DataFrame({'date': date, 'milestone': milestone}, index=[i])])
            add_row['MilestonesStatus']='DOWNLOADED'
            add_row['Milestones']=[milestone_df]
        except:
            add_row['MilestonesStatus']='DOWNLOAD_ERROR'
    else:
        add_row['MilestonesStatus']='MILESTONE_TAB_MISSING'

    #### Get Market Price timeseries

    if not skip_price:
    #     if 'driver' not in locals():
    #         driver = get_chromedriver(chromedriver_path=chromedriver_path)
    #         driver.get(url)
    #     series_status, series_df=get_price_series(driver)
        series_status='PRICE_TAB_MISSING'
        series_df=None
        try:
            if soup.find("div", class_="companyGraph") is not None:
                price_url=PRICE_API + url.replace(URL_ROOT, '').strip("/")
                price_page = requests.get(price_url)

                data=price_page.json()
                for col in ['prices', 'market_cap', 'h24_vol']:
                    df_t=pd.DataFrame(data[col], columns =['Date', col])
                    df_t['Date']=pd.to_datetime(df_t['Date'], unit='ms')
                    if series_df is not None:
                        series_df=series_df.merge(df_t, on='Date', how='left')
                    else:
                        series_df=df_t
                series_df.columns=['Date', 'PriceUSD', 'MarketCap', 'Volume24H']
                series_status='DOWNLOADED'
                if data['total'] != len(series_df):
                    series_status='DOWNLOADED_BUT_LENGTH_MISMATCH'
                add_row['MarketPriceSeries']=[series_df]
        except:
            series_status='DOWNLOAD_ERROR'
    else:
        series_status='DOWNLOAD_SKIPPED'

    add_row['MarketPriceSeriesStatus']=series_status
     
#     if 'driver' in locals():
#         driver.close()
        
    return add_row


def extract_scaping_icomarks(scrape_df):
    
    df_final=pd.DataFrame()    
    for index, row in scrape_df.iterrows():

        print(f'   - Processing {str(index + 1)} / {len(scrape_df)}', end='\r')

        if row['ScrapeStatus'] != 'ERROR':

            url=row['url']
            add_row=pd.DataFrame({'url': url}, index=[0])
            block_df=row['InfoBlock']

            #### unpack Icomarks rating

            add_row['RatingIcomarks']=row['Rating_TOTAL_SCORE']


            #### unpack Team size

            add_row['TeamSize']=row['TeamSize']
            add_row['AdvisorSize']=row['AdvisorSize']


            #### unpack General tab

            blk_gen=block_df[block_df['BlockName'] == 'General']
            blk_gen_item=blk_gen['Item'].values
            # bounty - dummy
            bounty= 1 if 'Bounty' in blk_gen_item else 0
            # MVP - dummy
            mvp= 1 if 'MVP' in blk_gen_item else 0
            # country
            country= blk_gen[blk_gen['Item'] == 'Country']['ItemValue'].values if 'Country' in blk_gen_item else ''
            # dates (usually is just TBA)
            dates=blk_gen[blk_gen['Item'] == 'Dates']['ItemValue'].values if 'Dates' in blk_gen_item else ''
            # ICO dates
            if 'ICO Time' in blk_gen_item:
                dt=blk_gen[blk_gen['Item'] == 'ICO Time']['ItemValue'].values[0]
                dt=[adjust_date(x.strip()) for x in dt.split('-')]
                try:
                    ico_date_start=pd.to_datetime(dt[0], infer_datetime_format=True)
                except:
                    ico_date_start=dt[0]
                try:
                    ico_date_end=pd.to_datetime(dt[1], infer_datetime_format=True)
                except:
                    ico_date_end=dt[1]
            else:
                ico_date_start=None
                ico_date_end=None
            # IEO dates
            if 'IEO Time' in blk_gen_item:
                dt=blk_gen[blk_gen['Item'] == 'IEO Time']['ItemValue'].values[0]
                dt=[adjust_date(x.strip()) for x in dt.split('-')]
                try:
                    ieo_date_start=pd.to_datetime(dt[0], infer_datetime_format=True)
                except:
                    ieo_date_start=dt[0]
                try:
                    ieo_date_end=pd.to_datetime(dt[1], infer_datetime_format=True)
                except:
                    ieo_date_end=dt[1]
            else:
                ieo_date_start=None
                ieo_date_end=None
            # IEO launchpad
            ieo_launch= blk_gen[blk_gen['Item'] == 'IEO launchpad']['ItemValue'].values if 'IEO launchpad' in blk_gen_item else ''
            # STO dates
            if 'STO Time' in blk_gen_item:
                dt=blk_gen[blk_gen['Item'] == 'STO Time']['ItemValue'].values[0]
                dt=[adjust_date(x.strip()) for x in dt.split('-')]
                try:
                    sto_date_start=pd.to_datetime(dt[0], infer_datetime_format=True)
                except:
                    sto_date_start=dt[0]
                try:
                    sto_date_end=pd.to_datetime(dt[1], infer_datetime_format=True)
                except:
                    sto_date_end=dt[1]
            else:
                sto_date_start=None
                sto_date_end=None
            # Pre-sale Time dates and dummy
            if 'Pre-sale Time' in blk_gen_item:
                pres_dummy=1
                dt=blk_gen[blk_gen['Item'] == 'Pre-sale Time']['ItemValue'].values[0]
                dt=[adjust_date(x.strip()) for x in dt.split('-')]
                try:
                    pres_date_start=pd.to_datetime(dt[0], infer_datetime_format=True)
                except:
                    pres_date_start=dt[0]
                try:
                    pres_date_end=pd.to_datetime(dt[1], infer_datetime_format=True)
                except:
                    pres_date_end=dt[1]
            else:
                pres_date_start=None
                pres_date_end=None
                pres_dummy=0
            # Website and dummy
            if 'Website' in blk_gen_item:
                website=blk_gen[blk_gen['Item'] == 'Website']['ItemUrl'].values
                website_dummy=1
            else:
                website=''
                website_dummy=0
            # Whitepaper and dummy
            if 'White paper' in blk_gen_item:
                whitepaper=blk_gen[blk_gen['Item'] == 'White paper']['ItemUrl'].values
                whitepaper_dummy=1
            else:
                whitepaper=''
                whitepaper_dummy=0
            # Whitelist/KYC
            if 'Whitelist/KYC' in blk_gen_item:
                if blk_gen[blk_gen['Item'] == 'Whitelist/KYC']['ItemValue'].values in ['KYC', 'Whitelist + KYC']:
                    kyc= 1 
                if blk_gen[blk_gen['Item'] == 'Whitelist/KYC']['ItemValue'].values in ['Whitelist', 'Whitelist + KYC']:
                    whitelist= 1
                if blk_gen[blk_gen['Item'] == 'Whitelist/KYC']['ItemValue'].values not in ['Whitelist', 'KYC', 'Whitelist + KYC']:
                    print(f'- unexpected value in "Whitelist/KYC" for {url}')
            else:
                kyc=0
                whitelist=0

            add_row['BountyDummy']=bounty
            add_row['MVPDummy']=mvp
            add_row['Country']=country
            add_row['Dates']=dates
            add_row['ICODateStart']=ico_date_start
            add_row['ICODateEnd']=ico_date_end
            add_row['IEODateStart']=ieo_date_start
            add_row['IEODateEnd']=ieo_date_end
            add_row['IEOLaunchpad']=ieo_launch
            add_row['STODateStart']=sto_date_start
            add_row['STODateEnd']=sto_date_end
            add_row['PreSaleDummy']=pres_dummy
            add_row['PreSaleDateStart']=pres_date_start
            add_row['PreSaleDateEnd']=pres_date_end
            add_row['WebsiteDummy']=website_dummy
            add_row['WebsiteUrl']=website
            add_row['WhitepaperDummy']=whitepaper_dummy
            add_row['WhitepaperUrl']=whitepaper
            add_row['KYCDummy']=kyc
            add_row['WhitelistDummy']=whitelist


            #### unpack Token info tab

            blk_tok=block_df[block_df['BlockName'] == 'Token info']
            blk_tok_item=blk_tok['Item'].values

            # ticker
            ticker= blk_tok[blk_tok['Item'] == 'Ticker']['ItemValue'].values if 'Ticker' in blk_tok_item else ''
            # platform
            platform= blk_tok[blk_tok['Item'] == 'Platform']['ItemValue'].values if 'Platform' in blk_tok_item else ''
            # Token Type and ERC20 - dummy
            toktype= blk_tok[blk_tok['Item'] == 'Token Type']['ItemValue'].values if 'Token Type' in blk_tok_item else ['']
            erc20= 1 if 'erc' in toktype[0].lower() else 0
            # available for sale
            avail_sale= blk_tok[blk_tok['Item'] == 'Available for sale']['ItemValue'].values if 'Available for sale' in blk_tok_item else ''
            # total supply
            tot_supply= blk_tok[blk_tok['Item'] == 'Total supply']['ItemValue'].values if 'Total supply' in blk_tok_item else ''        

            add_row['Ticker']=ticker
            add_row['Platform']=platform
            add_row['TokenType']=toktype
            add_row['ERC20Dummy']=erc20
            add_row['TokenAvailForSale']=avail_sale
            add_row['TokenTotSupply']=tot_supply


            #### unpack Financial tab

            blk_fin=block_df[block_df['BlockName'] == 'Financial']
            blk_fin_item=blk_fin['Item'].values

            # raised
            raised= blk_fin[blk_fin['Item'] == 'Raised']['ItemValue'].values if 'Raised' in blk_fin_item else ''
            # Hard/Soft cap
            hardcap= blk_fin[blk_fin['Item'] == 'Hard cap']['ItemValue'].values if 'Hard cap' in blk_fin_item else ''
            softcap= blk_fin[blk_fin['Item'] == 'Soft cap']['ItemValue'].values if 'Soft cap' in blk_fin_item else ''
            # accepted currency
            accepted= blk_fin[blk_fin['Item'] == 'Accepting']['ItemValue'].values if 'Accepting' in blk_fin_item else ''
            # bonuses dummy
            bonus= 1 if 'Bonuses' in blk_fin_item else 0
            # ICO, IEO, STO, Pre-Sale price
            ico_price= blk_fin[blk_fin['Item'] == 'ICO Price']['ItemValue'].values if 'ICO Price' in blk_fin_item else ''
            ieo_price= blk_fin[blk_fin['Item'] == 'IEO Price']['ItemValue'].values if 'IEO Price' in blk_fin_item else ''
            sto_price= blk_fin[blk_fin['Item'] == 'STO Price']['ItemValue'].values if 'STO Price' in blk_fin_item else ''
            pres_price= blk_fin[blk_fin['Item'] == 'Pre-sale Price ']['ItemValue'].values if 'Pre-sale Price ' in blk_fin_item else ''

            add_row['FundRaised']=raised
            add_row['FundHardCap']=hardcap
            add_row['FundSoftCap']=softcap
            add_row['AcceptedCurr']=accepted
            add_row['BonusDummy']=bonus
            add_row['ICOPrice']=ico_price
            add_row['IEOPrice']=ieo_price
            add_row['STOPrice']=sto_price
            add_row['PreSalePrice']=pres_price


            #### unpack Social Media tab

            social_series = 1 if row['SocialSeriesStatus'] == 'DOWNLOADED' else 0
            add_row['SocialSeriesDownloaded']=social_series
            
            blk_soc=block_df[block_df['BlockName'] == 'Social media']
            media_list=None
            if len(blk_soc) > 0:
                media_list=[blk_soc['ItemValue'].values.tolist()]

            add_row['SocialMedia']=media_list


            #### unpack Social Stats

    #         if not np.isnan(row['SocialWithRating']):

    #             track_social=row['SocialWithRating']

    #             if row['SocialSeriesStatus'] == 'DOWNLOADED':

    #                 blk_soc=row['SocialBlock']
    #                 stats=blk_soc[0]
    #                 timeseries_dict=blk_soc[0]['timeseries']
    #                 # todo: si possono calcolare medie, dev standard , ecc
    #         else:
    #             track_social=None

    #         add_row['TrackedSocial']=track_social
    
    
            #### unpack Trading Price data

            price_series = 1 if row['MarketPriceSeriesStatus'] == 'DOWNLOADED' else 0
            add_row['PriceSeriesDownloaded']=price_series
            
            # todo: si possono calcore medie, dev standard, ecc

            df_final=pd.concat([df_final, add_row])

    return df_final


def summary_stats(df=None, date_format='D', n_digits=2):

    '''
      date_format: show dates up to days https://numpy.org/doc/stable/reference/arrays.datetime.html#arrays-dtypes-dateunits
      n_digits: rounding digits for min, max, mean, ...
    '''

    import pandas as pd
    import numpy as np
    import warnings
    
    NUMERICS = ['number', 'int16', 'int32', 'int64', 'float16', 'float32', 'float64']
    DATES = ['datetimetz', 'datetime64', 'datetime']
    CATEGORICALS = ['object']
    BOOLS = ['bool']
    
    # remove columns that contains list or dataframes
    exclude_cols=[]
    for (colname,col) in df.items():
        types = set([type(x) for x in col])
        if any([x is list for x in types]) or any([x is pd.core.frame.DataFrame for x in types]):
            exclude_cols.append(colname)
    if exclude_cols:
        print(f'    - {len(exclude_cols)} columns excluded:', ', '.join(exclude_cols))
        df=df.drop(columns=exclude_cols)
    
    all_cols = df.columns

    tot_rows, tot_cols = df.shape
    pd.set_option('display.max_rows', tot_cols)

    num_cols = df.select_dtypes(include=NUMERICS).columns
    date_cols = df.select_dtypes(include=DATES).columns
    cat_cols = df.select_dtypes(include=CATEGORICALS).columns
    bool_cols = df.select_dtypes(include=BOOLS).columns

    # convert int64 to float (so as to evaluate mean, etc)
    for col in df.select_dtypes(include=['int64']).columns:
        df[col] = df[col].astype(float)    
    
    # Numerical stats
    num_stats = pd.DataFrame(columns=['VARIABLE', 'TYPE', 'OBS', 'UNIQUE', 'NANs', 'INFs', 'ZEROs', 'MIN', 'MAX', 'MEAN',
                                      'STDDEV', 'MEDIAN', 'PERC1', 'PERC5', 'PERC95', 'PERC99', 'SUM'])
    for var in num_cols:

        val = df[var].values
        val = val[~np.isnan(val)]
        val = val[~np.isinf(val)]

        warnings.filterwarnings("ignore")
        perc = np.quantile(val, [0.01, 0.05, 0.95, 0.99])
        median = np.round(np.median(val), n_digits)
        warnings.filterwarnings("default")

        add_row = pd.DataFrame({'VARIABLE': var,
                                'TYPE': str(df[var].dtypes),
                                'OBS': tot_rows,
                                'UNIQUE': df[var].nunique(),
                                'NANs': df[var].isna().sum(),
                                'INFs': np.isinf(df[var].values).sum(),
                                'ZEROs': sum(df[var] == 0),
                                'MIN': np.round(val.min(), n_digits),
                                'MAX': np.round(val.max(), n_digits),
                                'MEAN': np.round(val.mean(), n_digits),
                                'STDDEV': np.round(val.std(), n_digits),
                                'MEDIAN': median,
                                'PERC1': np.round(perc[0], n_digits),
                                'PERC5': np.round(perc[1], n_digits),
                                'PERC95': np.round(perc[2], n_digits),
                                'PERC99': np.round(perc[3], n_digits),
                                'SUM': np.round(val.sum(), n_digits)
                               }, index = [0])

        num_stats = pd.concat([num_stats, add_row])


    # Categorical stats
    cat_stats = pd.DataFrame(columns=['VARIABLE', 'TYPE', 'OBS', 'UNIQUE', 'NANs', 'BLANKs', 'VALUES_BY_FREQ'])

    for var in cat_cols:

        add_row = pd.DataFrame({'VARIABLE': var,
                                'TYPE': str(df[var].dtypes),
                                'OBS': tot_rows,
                                'UNIQUE': df[var].nunique(),
                                'NANs': df[var].isna().sum(),
                                'BLANKs': sum(df[var] == ''),
                                'VALUES_BY_FREQ': '|'.join(df[var].value_counts().index[:40].astype(str))
                               }, index = [0])

        cat_stats = pd.concat([cat_stats, add_row])


    # Boolean stats
    bool_stats = pd.DataFrame(columns=['VARIABLE', 'TYPE', 'OBS', 'NANs', 'MEAN', 'STDDEV', 'MEDIAN', 'PERC1',
                                       'PERC5', 'PERC95', 'PERC99', 'SUM', 'VALUES_BY_FREQ'])
    for var in bool_cols:

        val = df[var].dropna().values

        warnings.filterwarnings("ignore")
        perc = np.quantile(val.astype(int), [0.01, 0.05, 0.95, 0.99])
        median = np.round(np.median(val), n_digits)
        warnings.filterwarnings("default")        
                                                   
        add_row = pd.DataFrame({'VARIABLE': var,
                                'TYPE': str(df[var].dtypes),
                                'OBS': tot_rows,
                                'NANs': df[var].isna().sum(),
                                'MEAN': np.round(val.mean(), n_digits),
                                'STDDEV': np.round(val.std(), n_digits),
                                'MEDIAN': median,
                                'PERC1': perc[0],
                                'PERC5': perc[1],
                                'PERC95': perc[2],
                                'PERC99': perc[3],
                                'SUM': val.sum(),
                                'VALUES_BY_FREQ': ', '.join([str(k) + ': ' + str(v)
                                                             for k, v in df[var].dropna().value_counts().to_dict().items()])
                               }, index = [0])

        bool_stats = pd.concat([bool_stats, add_row])

    # Date stats
    date_stats = pd.DataFrame(columns=['VARIABLE', 'TYPE', 'OBS', 'UNIQUE', 'NANs', 'MIN', 'MAX', 'MEDIAN', 'PERC1',
                                       'PERC5', 'PERC95', 'PERC99'])
    for var in date_cols:

        val = df[var].dropna().values
        val_str = np.datetime_as_string(val, unit=date_format)

        # calculation for median and quantile
        val_cnt = sorted(pd.Series(val_str).unique())
        mapping = dict(zip(val_cnt, range(len(val_cnt))))
        mapped = [mapping.get(v, v) for v in val_str]
        warnings.filterwarnings("ignore")
        med_ind = np.median(mapped).astype(int)
        warnings.filterwarnings("default")  

        if len(val) > 0:
        
            warnings.filterwarnings("ignore")
            perc = np.quantile(mapped, [0.01, 0.05, 0.95, 0.99]).astype(int)
            median = [k for k, v in mapping.items() if v == med_ind][0]
            warnings.filterwarnings("default")  
            add_row = pd.DataFrame({'VARIABLE': var,
                                    'TYPE': str(df[var].dtypes),
                                    'OBS': tot_rows,
                                    'UNIQUE': df[var].nunique(),
                                    'NANs': df[var].isna().sum(),
                                    'MIN': np.datetime_as_string(val.min(), unit=date_format),
                                    'MAX': np.datetime_as_string(val.max(), unit=date_format),
                                    'MEDIAN': median,
                                    'PERC1': [k for k, v in mapping.items() if v == perc[0]][0],
                                    'PERC5': [k for k, v in mapping.items() if v == perc[1]][0],
                                    'PERC95': [k for k, v in mapping.items() if v == perc[2]][0],
                                    'PERC99': [k for k, v in mapping.items() if v == perc[3]][0]
                                   }, index = [0])
        else:
            add_row = pd.DataFrame({'VARIABLE': var,
                            'TYPE': str(df[var].dtypes),
                            'OBS': tot_rows,
                            'UNIQUE': df[var].nunique(),
                            'NANs': df[var].isna().sum()
                       }, index = [0])

        date_stats = pd.concat([date_stats, add_row])

    # final stats
    all_col_set = ['VARIABLE', 'TYPE', 'OBS', 'UNIQUE', 'NANs', 'INFs', 'ZEROs', 'BLANKs', 'MEAN', 'STDDEV', 'MIN', 
                                       'PERC1', 'PERC5', 'MEDIAN', 'PERC95', 'PERC99', 'MAX', 'SUM', 'VALUES_BY_FREQ']
    used_col_set = []
    final_stats = pd.DataFrame(columns=all_col_set)
    if num_stats.shape[0] > 0:
        final_stats = pd.concat([final_stats, num_stats])
        used_col_set.extend(num_stats.columns)
    if cat_stats.shape[0] > 0:
        final_stats = pd.concat([final_stats, cat_stats])
        used_col_set.extend(cat_stats.columns)
    if bool_stats.shape[0] > 0:
        final_stats = pd.concat([final_stats, bool_stats])
        used_col_set.extend(bool_stats.columns)
    if date_stats.shape[0] > 0:
        final_stats = pd.concat([final_stats, date_stats])
        used_col_set.extend(date_stats.columns)

    final_stats = final_stats[[x for x in all_col_set if x in np.unique(used_col_set)]]

    if final_stats['VARIABLE'].nunique() != final_stats.shape[0]:
        print('-- Duplicated variables found!')

    if final_stats['VARIABLE'].nunique() != tot_cols:
        print('-- Missing variables found:\n    ', '\n     '.join(set(all_cols) - set(final_stats['VARIABLE'].values)))

    final_stats['order'] = pd.Categorical(final_stats['VARIABLE'], categories = all_cols, ordered = True)
    final_stats = final_stats.sort_values(by='order').drop(columns=['order'])

    # add percentage to missing, inf, zeros, blank
    if 'NANs' in final_stats.columns:
        final_stats['NANs'] = [str(x) + ' (' +  str(np.round(x / tot_rows * 100, 1)) + '%)'
                               if not np.isnan(x) else '' for x in final_stats['NANs'].values]
    if 'INFs' in final_stats.columns:
        final_stats['INFs'] = [str(x) + ' (' +  str(np.round(x / tot_rows * 100, 1)) + '%)'
                               if not np.isnan(x) else '' for x in final_stats['INFs'].values]
    if 'ZEROs' in final_stats.columns:
        final_stats['ZEROs'] = [str(x) + ' (' +  str(np.round(x / tot_rows * 100, 1)) + '%)'
                               if not np.isnan(x) else '' for x in final_stats['ZEROs'].values]
    if 'BLANKs' in final_stats.columns:
        final_stats['BLANKs'] = [str(x) + ' (' +  str(np.round(x / tot_rows * 100, 1)) + '%)'
                               if not np.isnan(x) else '' for x in final_stats['BLANKs'].values]
    final_stats.fillna('', inplace=True)
    
    return final_stats

def convert_fund(x):
    
    if len(x) > 0:
        if '$' in x:
            out=int(float(x.replace('$', '').replace(',', '').strip()))
        else:
            out=x
    else:
        out=None
    
    return out

def extract_price(row):
    
    CURRENCY=['USD', 'EUR', 'ETH', 'BNB', 'USDT', 'BTC', 'CHF']
    
    error=''
    warning=''
    ticker=row['Ticker']
    price=(row['Price'].replace('$', 'USD').replace('€', 'EUR').replace('\u200b', '').replace('~', '')           
           .replace('.000.000.000', '000000000').replace('00.000.000', '00000000').replace('0.000.000', '0000000')
             .replace('.000.000', '000000').replace('00.000', '00000'))
    
    # split token and currency
    if ticker == '':
        ticker='xxxx1111yyyy'
    split_str=re.split('==|≈|=|—|:|＝', price)
    if len(split_str) == 1 and ticker in price:
        split_str=re.split('-', price)
    token_ind=[i for i, x in enumerate(split_str) if ticker in x and len(x)>0]
    currency_ind=[i for i, x in enumerate(split_str) if ticker not in x and len(x)>0]
    if len(token_ind) > 1 or len(currency_ind) > 1:
        retrieve_currency_ind=np.unique([i for i, y in enumerate(split_str) for x in CURRENCY if x in y]).tolist()
        if len(retrieve_currency_ind) == 1:
            currency_ind=retrieve_currency_ind
            token_ind=list(set(range(len(split_str))) - set(currency_ind))
            warning+='-multiple token or currency index recovered'
        else:
            error+='-multiple token or currency index '
    token_part= split_str[token_ind[0]] if token_ind else ''
    currency_part= split_str[currency_ind[0]] if currency_ind else ''

    # extract unit and currency
    token_unit=''
    if len(token_part) > 0:
        token_unit=token_part.replace(ticker, '').strip()
        if token_unit.startswith('0,'):
            token_unit=token_unit.replace('0,', '0.')
        else:
            token_unit=token_unit.replace(',', '')
        if re.sub('[A-Za-z]+', '', token_unit) != token_unit:   # try to remove mismatch in ticker
            token_unit=re.sub('[A-Za-z]+', '', token_unit)
            warning+='-adjusted ticker'
    if len(token_unit) == 0:
        token_unit='1'
    token_unit_num=None
    token_unit=token_unit.replace(' ', '').replace('[', '').replace(']', '')
    if '-' in token_unit:    # price range -> take average
        try:
            token_unit_num=sum([float(x) for x in token_unit.split('-')])/len(token_unit.split('-'))
            warning+='-token average'
        except:
            error+='-token average failed'
    else:
        try:
            token_unit_num=float(token_unit)
        except:
            error+='-token error float'
        if '-token error float' in error:   # try to remove . used as thousand separator
            try:
                token_unit_num=float(token_unit.replace('.000', '000'))
                warning+='-token dot thousand separator removed'
                error=error.replace('-token error float', '')
            except:
                pass
            
    currency_unit=None
    currency_lab=None
    currency_unit_num=None
    currency_part=currency_part.replace(' ', '')
    if len(currency_part) > 0:
        # check if decimal is . or ,
        if len(re.findall('[^0-9]0,', currency_part)) > 0 or currency_part.startswith('0,'):
            currency_part=currency_part.replace('0,', '0.')
        else:
            currency_part=currency_part.replace(',', '')
        currency_split=re.findall("(\d*\.?\d+|[A-Za-z]+)", currency_part)
        letters_count=[sum([x.isalpha() for x in y]) for y in currency_split]
        if len(currency_split) != 2:

            lab_ind=[i for i, x in enumerate(letters_count) if x > 0]
            check_lab=np.unique(np.array(currency_split)[lab_ind])
            if len(check_lab) > 1:
                error+='-currency range error multiple labels'
            elif len(check_lab) == 0:
                error+='-currency range error missing label'
            else:
                currency_lab=np.unique(np.array(currency_split)[lab_ind])[0]
                currency_unit=currency_part.replace(currency_lab, '').replace(' ', '')
                if '-' in currency_unit:    # price range -> take average
                    try:
                        currency_unit_num=sum([float(x) for x in currency_unit.split('-')])/len(currency_unit.split('-'))
                        warning+='-currency average'
                    except:
                        error+='-currency range error float'

        else:
            number_ind=letters_count.index(min(letters_count))
            lab_ind=letters_count.index(max(letters_count))
            currency_lab=currency_split[lab_ind]
            currency_unit=currency_split[number_ind].replace(' ', '')
            if currency_unit.startswith('0,'):
                currency_unit=currency_unit.replace('0,', '0.')
            else:
                currency_unit=currency_unit.replace(',', '')
            try:
                currency_unit_num=float(currency_unit)
            except:
                error+='-currency error float'
            if '-currency error float' in error:   # try to remove . used as thousand separator
                try:
                    currency_unit_num=float(currency_unit.replace('.000', '000'))
                    warning+='-currency dot thousand separator removed'
                    error=error.replace('-token error float', '')
                except:
                    pass
    
    if token_unit_num is None:
        error+='-missing token unit numeric'
    if currency_unit_num is None:
        error+='-missing currency unit numeric'
        
    
    return pd.Series({'token_unit': token_unit, 'token_unit_num': token_unit_num,
                      'currency_unit': currency_unit, 'currency_unit_num':currency_unit_num, 'currency_lab': currency_lab,
                      'token_part': token_part, 'currency_part': currency_part, 'error': error[1:], 'warning': warning[1:]})


def extract_platform(x):
    
    x = x.replace('Unknown', '')
    x = x.replace('Own Platform', 'oooo')
    x = x.replace('Own', 'oooo')
    x = x.replace('oooo', 'Own Platform')
    x = re.sub('\s{2,}', '', x)
    x = x.replace('POS', 'PoS').replace('POW', 'PoW')
    x = x.replace('Stellar Asset', 'Stellar').replace('Stellar Lumens', 'Stellar').replace('Stellar Token', 'Stellar')
    x = x.replace(',', '___').replace('/', '___').split('___')
    x = [y.strip() for y in x]
    
    return x


def extract_TokenAvailForSale(row):
    
    x = row['TokenAvailForSale'].replace('–', '-').replace('\xa0', '')#.encode("ascii", "ignore").decode()
    ticker = row['Ticker']
    url = row['url']
    warn=''
    error=''
    if x != '':
        x = x.split('(')
        if '%' in x[-1]:
            token_perc=x[-1].replace(')', '')
            token_perc=token_perc.replace('~', '').replace(',', '.').replace('%', '')
    #         sep_list=['–', '-']
            if '-' in token_perc: #any([sep in token_perc for sep in sep_list]):
                warn+='|perc with separator'
                select_sep='-' #list(set(token_perc) & set(sep_list))[0]
                token_perc=np.average([float(y) for y in token_perc.split(select_sep)])
            else:
                token_perc=float(token_perc)
            num_part='('.join(x[:-1])
        else:
            token_perc=None        
            num_part='('.join(x)
            warn+='|no perc available'

        if ticker == '300':
            num_part='300'
        elif ticker != '':
            if ticker in num_part:
                num_part=num_part.split(ticker)[0].strip()
            else:
                num_part=num_part.strip()
                warn+='|no ticker in TokenAvailForSale'
        else:
            warn+='|missing Ticker'
            
        try:
            num_part=int(num_part.replace(',', ''))
        except:
            num_part=None
            error+='|error in converting numeric part for TokenAvailForSale'
    else:
        token_perc=None
        num_part=None
    
    x = row['TokenTotSupply']
    if x != '':
        if ticker == '300':
            num_part_suppl='300'
        elif url == 'https://icomarks.ai/ico/getthebit':
            num_part_suppl='777,000,000.00'
        elif ticker != '':
            if ticker in x:
                num_part_suppl=x.split(ticker)[0].strip()
            else:
                
                num_part_suppl=x.strip()
                warn+='|no ticker in TokenTotSupply'
        else:
            warn+='|missing Ticker'
            
        try:
            num_part_suppl=int(num_part_suppl.replace(',', '').split('.')[0])
        except:
            num_part_suppl=None
            error+='|error in converting numeric part for TokenTotSupply'
    else:
        num_part_suppl=None
    
    return pd.Series({'token_perc': token_perc, 'num_part': num_part, 'num_part_suppl': num_part_suppl,
                      'warn': warn[1:], 'error': error[1:]})


def extract_AcceptedCurr(x):
    
    x=x.upper()
    x=x.replace('–', '-').encode("ascii", "ignore").decode()
    x=x.replace('TRC-20', '').replace('TRC20', '').replace('ERC20', '').replace('BEP20', '').replace('BEP 20', '')
    x=x.replace('$', '').replace('()', '').replace('FIAT:', '').replace('CRYPTO:', '').replace('FIAT (', '').replace('PAYMENTS', '').replace('FIATS', 'FIAT')
    x=x.replace('&', ',').replace('/', ',').replace('-', ',').replace('.', ',').replace(';', ',').replace('AND ', ',').replace('OR ', ',')
    x=(x.replace('AS WELL AS ETH', 'ETH').replace('BINANCE COIN (BNB)', 'BNB').replace('BITCOIN (BTC)', 'BTC')
       .replace('BITCOIN CASH', 'BCH').replace('BITCOIN CASH (BCH)', 'BCH').replace('BITCOIN SV', 'BSV')
       .replace('BNB COIN', 'BNB').replace('BTC CASH', 'BCH').replace('BURST COIN', 'BURST').replace('BCH (BCH)', 'BCH')
       .replace('BNB (BSC)', 'BNB').replace('CARDANO (ADA)', 'ADA').replace('COINBASE COMMERCE', 'COINBASE')
       .replace('CREDIT CARD', 'CREDIT').replace('DASH (DASH)', 'DASH').replace('DEBIT CARD', 'DEBIT')
       .replace('DEBIT CARDS', 'DEBIT').replace('DIRECT BANK DEPOSIT', 'BANKDEPOSIT').replace('DKK WITH GBP', 'FIAT')
       .replace('ETHEREUM (ETH)', 'ETH').replace('EURO VIA BANK TRANSFER)', 'BANKTRANSFER').replace('GAS (NEO BLOCKCHAIN CRYPTOCURRENCY)', 'GAS')
       .replace('LITECOIN (LTC)', 'LTC').replace('OTHER STABLE COINS', 'OTHER').replace('OVER 50 OTHER CURRENCIES', 'OTHER')
       .replace('PAXOS STANDARD TOKEN', 'PAX').replace('PERFECT MONEY(PM)', 'PM').replace('PHYSICAL GOLD', 'GOLD').replace('PRESALE ONLY)', '')
       .replace('RIPPLE (XRP)', 'XRP').replace('SRMINER (SRM)', 'SRM').replace('DIGIBYTE (DGB)', 'DGB').replace('STABLE COINS AS USDT', 'USDT')
       .replace('TETHER (USDT)', 'USDT').replace('TETHER', '').replace('TRON (TRX)', 'TRON').replace('(PAYPAL)', '').replace('(VISA', '')
       .replace('USD WIRE TRANSFER', 'USD').replace('VISA + MASTER CARD IN EUR', 'EUR').replace('WIRE TRANSFER', 'WIRE')
       .replace('OTHER CRYPTOCURRENCIES', 'OTHER').replace('ALTCOINS', 'ALT').replace('CARDS', 'CARD')
       .replace('DEBIT', 'CARD').replace('CREDIT', 'CARD').replace('MASTERCARD', 'CARD').replace('VISA', 'CARD')
       .replace('BITCOIN', 'BTC').replace('BITSHARES', 'BTS').replace('DOGECOIN', 'DOGE').replace('ETHER', 'ETH')
       .replace('ETHEREUM', 'ETH').replace('KICKCOIN', 'KICK').replace('LITCOIN', 'LTC').replace('LITECOIN', 'LTC')
       .replace('VARIOUS', 'OTHER').replace('VAROUS', 'OTHER').replace('WAVES', 'WAVE').replace('ZCASH', 'ZEC').replace('ETHEUM', 'ETH')
       .replace('TRUEUSD', 'TUSD').replace('OTHERS', 'OTHER').replace('BNB(BSC)', 'BSC').replace('BANK CARD', 'CARD')
      )
    x=x.replace('(', '').replace(')', '')
    x=re.sub('\d{2,}', '', x)
    x=x.replace(' ', ',')
    x=x.split(',')
    x=[y.strip() for y in x]
    x=[y for y in x if len(y) > 1]
    x=list(set(x))
    
    return x


def format_columns(format_df, cat_list=None, format_df_rows=0, results_folder=''):

    #### format FundRaised


    format_df.loc[format_df['url']=='https://icomarks.ai/ico/cdrx', 'FundRaised']='19,000,000$'

    print('\n** Formatting "FundRaised"')
    missing_pre=sum(format_df['FundRaised']=='')
    format_df['FundRaised']=format_df['FundRaised'].map(convert_fund)
    format_df=format_df.rename(columns={'FundRaised': 'FundRaisedUSD'})
    missing_post=format_df['FundRaisedUSD'].isna().sum()
    if missing_pre != missing_post:
        print(f'- FundRaisedUSD: expected missing: {missing_pre}  current missing: {missing_post}')
    if format_df.shape[0] != format_df_rows:
        print('########## "format_df" expected rows do not match')


    #### extract continent and subregion from country

    COUNTRY_MAPPING='.\\Data and papers\\country_to_continent.csv'  # https://worldpopulationreview.com/country-rankings/list-of-countries-by-continent
    MANUAL_MAPPING=pd.DataFrame({'original': ['ZBG', 'UK', 'USA', 'Global', 'Holland', 'Austria/Romania',
                                             'Delaware USA', 'CHE', 'Riga', 'UAE', 'Россия',
                                             'Wordwide', 'Kosovo', 'Düsseldorf', 'UAE, USA', 'World', 'BVI',
                                             'Amsterdam', 'London', 'Various', 'Latin America', 'England, UK',
                                             'Worldwide', 'DUBAI', 'Polska', 'Türkiye', 'Dubai',
                                             'UAE, Dubai', 'ShangHai', 'Toronto', 'Czechia', 'Scotland', 'Brasilia',
                                             'usa', 'Swaziland', 'ENGLAND', 'International', 'Italia', 'England',
                                             'Boulder, CO, San Francisco, CA, and Hyderabad, India', 'Slovak Republic',
                                             'Dubai, United Emirates', 'Ukraine, Finland, India', 'India, Nigeria and US',
                                             'USA, Canada, Philippines', 'US', 'Albuquerque', 'Istanbul', 'London, UK'],
                               'new': ['Hong Kong', 'United Kingdom', 'United States', 'Worldwide', 'Netherlands', 'Austria',
                                       'United States', 'Switzerland', 'Latvia', 'United Arab Emirates', 'Russia',
                                      'Worldwide', 'Albania', 'Germany', 'United Arab Emirates', 'Worldwide', 'British Virgin Islands',
                                      'Netherlands', 'United Kingdom', 'Worldwide', 'Brazil', 'United Kingdom',
                                      'Worldwide', 'United Arab Emirates', 'Poland', 'Turkey', 'United Arab Emirates',
                                      'United Arab Emirates', 'China', 'Canada', 'Czech Republic', 'United Kingdom', 'Brazil',
                                      'United States', 'Eswatini', 'United Kingdom', 'Worldwide', 'Italy', 'United Kingdom',
                                      'United States', 'Slovakia',
                                      'United Arab Emirates', 'Ukraine', 'India',
                                      'United States', 'United States', 'United States', 'Turkey', 'United Kingdom']})

    mapping=pd.read_csv(COUNTRY_MAPPING)[['country', 'region', 'subregion']].drop_duplicates()
    mapping=pd.concat([mapping, pd.DataFrame({'country': 'Worldwide', 'region': 'All', 'subregion': 'All'}, index=[mapping.shape[0]])])
    choices=mapping['country'].values.tolist()

    print('\n** Formatting "Country"')
    df_mapping=pd.DataFrame({'Country': format_df[format_df['Country'] != '']['Country'].unique()})
    df_mapping['Country_adj']=df_mapping['Country']
    for _, row in MANUAL_MAPPING.iterrows():
        df_mapping['Country_adj'].replace(row['original'], row['new'], inplace=True)
    df_mapping=df_mapping[df_mapping['Country_adj'] != '']
    df_mapping[['country', 'accuracy']]=df_mapping['Country_adj'].apply(lambda x: pd.Series(process.extractOne(x, choices)))
    print('- Mapped countries with low accuracy:')
    display(df_mapping.query('accuracy < 90').sort_values(by='accuracy'))

    format_df=format_df.merge((df_mapping.merge(mapping, on='country', how='left')[['Country', 'country', 'region', 'subregion']]
                               .rename(columns={'region': 'Region', 'subregion': 'SubRegion', 'country': 'country_fixed'})), on='Country', how='left')
    format_df[['Region', 'SubRegion']] = format_df[['Region', 'SubRegion']].fillna(value='')
    move_col = format_df.pop('Region')
    format_df.insert(format_df.columns.get_loc("Country")+1, 'Region', move_col)
    move_col = format_df.pop('SubRegion')
    format_df.insert(format_df.columns.get_loc("Region")+1, 'SubRegion', move_col)
    move_col = format_df.pop('country_fixed')
    format_df.insert(format_df.columns.get_loc("Country")+1, 'country_fixed', move_col)
    format_df.rename(columns={'Country': 'CountryOriginal', 'country_fixed': 'Country'}, inplace=True)
    if format_df.shape[0] != format_df_rows:
        print('########## "format_df" expected rows do not match')

       
    #### dummy for social media

    print('\n** Formatting "SocialMedia"')
    unique, counts = np.unique([item for sublist in format_df['SocialMedia'].dropna() for item in sublist], return_counts=True)
    social_count=pd.DataFrame({'val': unique, 'count': counts}).sort_values(by='count', ascending=False)
    print('- Counts for SocialMedia dummy:')
    display(social_count)

    df=format_df[['url', 'SocialMedia']].copy().dropna()
    df_dummy=pd.DataFrame()
    for _, row in df.iterrows():
        dt=pd.get_dummies(row['SocialMedia'], drop_first=False, prefix='Social')
        dt.insert(0, 'url', row['url'])
        df_dummy=pd.concat([df_dummy, dt.groupby('url').sum()])
    df_dummy=df_dummy.fillna(0).add_suffix('Dummy')
    df_dummy.columns=df_dummy.columns.str.replace('_', '')
    if df_dummy.max().max() > 1:
        print('#### Warning: dummy for social media exceed 1')
    social_lab=df_dummy.columns
    df_dummy.reset_index(inplace=True)
    format_df=format_df.merge(df_dummy, on='url', how='left').drop(columns=['SocialMedia'])
    format_df[social_lab] = format_df[social_lab].fillna(value=0)
    if format_df.shape[0] != format_df_rows:
        print('########## "format_df" expected rows do not match')


    #### convert Price to USD

    CURRENCY_ADJUST=pd.DataFrame({'original': ['ETHER', 'ETHERS', 'EHT', 'XML', 'USDUSD', 'EURO', 'EUREUR', 'WAVE',
                                               'USDBNB', 'USDBUSD', 'EH'],
                                'adjusted': ['ETH', 'ETH', 'ETH', 'XLM', 'USD', 'EUR', 'EUR', 'WAVES',
                                             'BNB', 'BUSD', 'ETH']})

    format_df.loc[format_df['Ticker']=='YFNFT', 'ICOPrice']='0.05 USD'
    format_df.loc[format_df['Ticker']=='YFNFT', 'TokenTotSupply']='73339 YFNFT'
    format_df.loc[format_df['Ticker']=='PRO6, PSIX', 'IEOPrice']='0.075 USD'
    format_df.loc[format_df['Ticker']=='ROX/ROX', 'ICOPrice']='0.05 USD'
    format_df.loc[format_df['Ticker']=='BMBCoin', 'ICOPrice']='0.1 USD'
    format_df.loc[format_df['Ticker']=='W2TW', 'ICOPrice']='0.65 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/blockchainaero', 'ICOPrice']='0.0002 ETH'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/abr', 'ICOPrice']='0.5 USD'
    format_df.loc[format_df['Ticker']=='WISE', 'ICOPrice']='15 WISE = 1 ETH'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/asseta', 'Ticker']='AST'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/future-coin-light', 'Ticker']='FTC'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/future-coin-light', 'Ticker']
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/cointour', 'ICOPrice']='1 COT = 0.0001 ETH'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/portalverse', 'ICOPrice']='1 PORV = 10 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/floppa-token', 'ICOPrice']='$6.3 = 1000 FLOP'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/hybrid-betting', 'ICOPrice']='1 HYB = 0.17$'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/bitcoincopy', 'ICOPrice']='0.31$ = BTCC'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/izombie-universe', 'ICOPrice']='1 BNB = 5800 iZOMBIE'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/syndiqate', 'ICOPrice']='1 SQAT = $0.20'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/metacade', 'ICOPrice']='0.012 USDT = 1 MCADE'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/verifi-defi', 'ICOPrice']='$0.004'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/open-box-swap', 'ICOPrice']='1 OBOX= 0.0225 BNB'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/suapp', 'ICOPrice']='1 SUP =  0.025 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/onlycumies', 'ICOPrice']='1 BNB = 1,000,000 ONLYCUMIES'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/calvaria', 'ICOPrice']='1 USDT = 100.0 RIA'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/one-game', 'ICOPrice']='$0.0075 = OGT'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/mcash-exchange-mbcash-ico', 'ICOPrice']='1 MBCash = $0.05'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/fightout', 'ICOPrice']='40 FGHT = 1 USDT'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/yesil-token', 'ICOPrice']='$0.0020'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/digital-dollar', 'ICOPrice']='$0.10'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/juniverse-token', 'ICOPrice']='$1'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/calmoairphoenix', 'ICOPrice']='0.01 $'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/promodio', 'ICOPrice']='$0.005'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/ocreata', 'ICOPrice']='1 BNB = 1,000,000 OCREATA'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/digital-euro', 'ICOPrice']='0.10 EUR'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/kaon', 'ICOPrice']='$0.0005'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/terrarium', 'ICOPrice']='1 TRM = 0.0005 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/blockapp', 'ICOPrice']='1 USDT = 1000 BAPP'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/bellatrix-network', 'ICOPrice']='330,000 BTX = 1 BNB'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/gisc-loancoin-network', 'ICOPrice']='20,000 GISC LoanCoin/GIS = 1 ETH'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/astra', 'ICOPrice']='1 ETH = 1,000 STAR'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/nodis', 'ICOPrice']='NODIS = 0.1105 GA'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/sensing-exchange-capital', 'ICOPrice']='1 SEC == 0.00166 BNB'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/playrs', 'ICOPrice']='1 ETH = 4,000 PLAY'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/lucisdollar', 'ICOPrice']='1 BTC = 400000000 LUCD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/beuthereum', 'Ticker']='BCH'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/lottechain', 'Ticker']='LEN'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/revenue-coin', 'ICOPrice']='1 RVC = 0.012 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/metadollar', 'ICOPrice']='1 USDME = $0.02'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/interledgerswap', 'ICOPrice']='1 BTC = 43049.896370 XRP'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/safemoonred', 'ICOPrice']='25000 SMR = 1 BNB'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/bonzai-technology', 'ICOPrice']='3,750,000,000 BONZAI = 1 BNB'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/zeller', 'ICOPrice']='1 ZLR = 0.0000165012 BNB'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/monkeycola', 'ICOPrice']='1 MKC = 0.3 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/metarobotwarrior', 'ICOPrice']='1 MRTW = 0.0001 BNB'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/wordpool', 'ICOPrice']='500000 WORD = 1 BNB'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/metahex', 'ICOPrice']='1,000 MTX = 8 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/hive-power', 'ICOPrice']='1 ETH = 2432 HVT'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/shibacute', 'ICOPrice']='1 BNB = 500000000000 SCUTE'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/coin-for-nature', 'ICOPrice']='1 BNB = 3200000 COFN'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/flexq', 'ICOPrice']='1 BNB = 5210 FLQ'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/velic', 'ICOPrice']='0.01 VELT = 1 USDT'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/civil', 'ICOPrice']='1 CIV = 0.0001 ETH'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/alsonft', 'ICOPrice']='1 AlsoNFT = 0.01 USDC'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/wiki-simplify', 'ICOPrice']='1 WSY = 0.15 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/2021coin', 'ICOPrice']='0.64 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/clear-crystal-token', 'ICOPrice']='0.0001 ETH'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/romad-endpoint-defense', 'ICOPrice']='1 RBDT = 0.00288000 USDT'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/crypto-market-ads', 'ICOPrice']='1 CMA coin = 0.01 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/custodi', 'ICOPrice']='0.60 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/micropad', 'ICOPrice']='0.075 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/neutro', 'IEOPrice']='1 NTO = 0.6 USDT'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/genevieve-vc1518213868', 'ICOPrice']='1 GXVC = 0.10 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/ixoracoin', 'ICOPrice']='1 BNB = 12,500,000 IXO'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/Apillon', 'IEOPrice']='0,40 $'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/earth-token', 'ICOPrice']='4000 ETN = 0.1 BTC'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/wall-street-coin', 'ICOPrice']='1 WSC = 0.75 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/globetrotter', 'ICOPrice']='1 GTT = 0.20 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/bluetherium', 'ICOPrice']=''
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/safecrypt', 'ICOPrice']='1 SFC =  0.00006 ETH'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/plaak', 'ICOPrice']='1 PLK = 1.4174 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/quanta-networks', 'ICOPrice']='1 QN = 0.70 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/true-gold-coin', 'ICOPrice']='1 TGC = 17.36 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/gauss-gang', 'ICOPrice']='1 GANG = 0.07 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/rpstoken', 'ICOPrice']='1 RPS= 0.000002 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/dragonnest', 'ICOPrice']='1 DRAGONNEST = 0.1 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/holiday', 'ICOPrice']='1 NEO = 1000 Holiday Coin'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/ip-gold', 'ICOPrice']='1 IPG = 1 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/flexion', 'ICOPrice']='1 FXN = 0.025 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/zantepay', 'ICOPrice']='1 ZNX = 0.310000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/christ-coins', 'ICOPrice']='1 CCLC = 0.09 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/coingrid', 'ICOPrice']='1 CGT = 0.044396 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/digitoads', 'ICOPrice']='0.02 USDT = 1 $TOADS'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/the-blue-eden', 'ICOPrice']='1 USDT = 12500 BLUE'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/ethaverse', 'ICOPrice']='1 ETH = 25000000 ETHA'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/comearth', 'ICOPrice']='0.006 ECOM'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/the-hack-fund', 'STOPrice']='1 HACK = 1 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/pepewx', 'ICOPrice']='0.0000075 USDT'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/capymagi-world', 'ICOPrice']='1 CMAGI = 0.006 USDT'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/xgames', 'ICOPrice']='1 XG = 1 USDT'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/deelance', 'ICOPrice']='1 DLANCE = 0.03 USDT'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/harambe-ai', 'ICOPrice']='1 Harambe AI = 0.125 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/richie-coin', 'ICOPrice']='1 RHPC = 0.00000012 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/galobank', 'ICOPrice']='$0.20 = 1 GALO'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/kurds', 'ICOPrice']='1 Kurds = $0.009000999999999999'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/cashfinanceapp', 'ICOPrice']='1 CFE = 0.000115 USDT'

    print('\n** Formatting "ICOPrice", "IEOPrice", "STOPrice"')
    check_single_value_in_row=format_df[['ICOPrice','IEOPrice', 'STOPrice']].apply(lambda x: sum(x!=''), axis=1).max()
    if check_single_value_in_row > 1:
        print('- Rows with more than one "ICOPrice", "IEOPrice", "STOPrice"')
    date_df=format_df[[col for col in format_df.columns if 'date' in col.lower()]].replace('TBA', '').replace(np.nan, '').fillna(pd.to_datetime('1 Jan 2150'))
    date_df['RefDate']=date_df.apply(lambda x: min([y for y in x if type(y) != str]), axis=1)
    # date_df.replace(pd.to_datetime('1 Jan 2150'), np.nan, inplace=True)
    price_df=format_df[['url', 'Ticker', 'ICOPrice','IEOPrice', 'STOPrice']].copy()
    price_df=pd.concat([price_df, date_df['RefDate']], axis=1)
    price_df['RefDate']=price_df['RefDate'].dt.year.astype(str)+'_'+price_df['RefDate'].dt.month.astype(str).str.zfill(2)
    price_df['RefDate'].replace('2150_01', 'last_avail', inplace=True)
    price_df['Price']=price_df['ICOPrice']+price_df['IEOPrice']+price_df['STOPrice']
    price_df=price_df[price_df['Price']!='']
    price_df['check_split']=price_df.apply(lambda x: len(re.split('==|≈|=|—', x['Price'])), axis=1)
    check_len=price_df.query('check_split > 2')
    if check_len.shape[0] > 0:
        print('- Found rows with wrong entries ("PriceUSD" will not be evaluated):')
        display(check_len)
        price_df=price_df[~price_df['url'].isin(check_len['url'].values)]
    price_df=pd.concat([price_df, price_df.apply(extract_price, axis=1)], axis=1)
    print('- Errors when parsing "ICOPrice", "IEOPrice", "STOPrice":')
    display(price_df['error'].value_counts().to_frame())
    price_df.to_csv(os.path.join(results_folder, '01a_c_ICOmarks_formatted_price_error_log.csv'), index=False, sep=';')
    print('- Error log saved in', os.path.join(results_folder, '01a_c_ICOmarks_formatted_price_error_log.csv'))
    price_df=price_df[price_df['error'] == '']
    price_df['currency_lab']=price_df['currency_lab'].str.upper()
    for i, row in CURRENCY_ADJUST.iterrows():
        price_df['currency_lab'].replace(row['original'], row['adjusted'], inplace=True)
    price_df=price_df.merge(cat_list[['url', 'StartDate']], on='url', how='left')  # check if "last_avail" is assigned only on TBA
    if not (price_df[price_df['RefDate']=='']['StartDate'].unique()=='TBA').all():
        print('- Warning: check missing "RefDate", "TBA" as date is automatically assigned and last available FX rate is taken')
    # ETH-USD https://finance.yahoo.com/quote/ETH-USD/history
    # BNB-USD https://finance.yahoo.com/quote/BNB-USD/history
    # USDT-USD https://finance.yahoo.com/quote/USDT-USD/history
    # BTC-USD https://finance.yahoo.com/quote/BTC-USD/history
    # XLM-USD https://finance.yahoo.com/quote/XLM-USD/history
    # TRX-USD https://finance.yahoo.com/quote/TRX-USD/history
    # BUSD-USD https://finance.yahoo.com/quote/BUSD-USD/history
    # KRW-USD https://finance.yahoo.com/quote/KRW-USD/history
    # NEO-USD https://finance.yahoo.com/quote/NEO-USD/history
    # MATIC-USD https://finance.yahoo.com/quote/MATIC-USD/history
    # USDC-USD https://finance.yahoo.com/quote/USDC-USD/history
    # WAVES-USD https://finance.yahoo.com/quote/WAVES-USD/history
    # BTS-USD https://finance.yahoo.com/quote/BTS-USD/history
    # VET-USD https://finance.yahoo.com/quote/VET-USD/history
    # ICX-USD https://finance.yahoo.com/quote/ICX-USD/history
    # LNC-USD https://finance.yahoo.com/quote/LNC-USD/history
    # WETH-USD https://finance.yahoo.com/quote/WETH-USD/history
    # DOT-USD https://finance.yahoo.com/quote/DOT-USD/history
    # AVAX-USD https://finance.yahoo.com/quote/AVAX-USD/history
    # AV-USD https://finance.yahoo.com/quote/AV-USD/history
    # EUR-USD https://www.investing.com/currencies/eur-usd-historical-data
    # GBP-USD https://www.investing.com/currencies/gbp-usd-historical-data
    # CHF-USD https://www.investing.com/currencies/chf-usd-historical-data
    # AUD-USD https://www.investing.com/currencies/aud-usd-historical-data
    # SGD-USD https://www.investing.com/currencies/sgd-usd-historical-data
    fx_df=pd.DataFrame()
    # from yahoo finance
    for file in ['ETH-USD', 'BNB-USD', 'USDT-USD', 'BTC-USD', 'XLM-USD', 'TRX-USD', 'BUSD-USD', 'KRW-USD', 'NEO-USD',
                 'MATIC-USD', 'USDC-USD', 'WAVES-USD', 'BTS-USD', 'VET-USD', 'ICX-USD', 'LNC-USD', 'WETH-USD',
                 'DOT-USD', 'AVAX-USD', 'AV-USD']:
        dd=pd.read_csv(os.path.join('.\\Data and papers\\FX', file+'.csv'), parse_dates=['Date'])
        dd.insert(0, 'currency_lab', file.replace('-USD', ''))
        dd['RefDate']=dd['Date'].dt.year.astype(str)+'_'+dd['Date'].dt.month.astype(str).str.zfill(2)
        dd['USDFx']=(dd['Open']+dd['Adj Close']) / 2
        fx_df=pd.concat([fx_df, dd[['currency_lab', 'RefDate', 'USDFx']]])
    # from investing.com
    for file in ['CHF_USD Historical Data', 'EUR_USD Historical Data', 'GBP_USD Historical Data',
                 'AUD_USD Historical Data', 'SGD_USD Historical Data']:
        dd=pd.read_csv(os.path.join('.\\Data and papers\\FX', file+'.csv'), parse_dates=['Date'])
        dd.insert(0, 'currency_lab', file.replace('_USD Historical Data', ''))
        dd['RefDate']=dd['Date'].dt.year.astype(str)+'_'+dd['Date'].dt.month.astype(str).str.zfill(2)
        dd['USDFx']=dd['Price']
        fx_df=pd.concat([fx_df, dd[['currency_lab', 'RefDate', 'USDFx']]])    
    fx_df=fx_df.groupby(['currency_lab', 'RefDate'], as_index=False).first()    # last month can have multiple values
    fx_df=pd.concat([fx_df,                                                     # average last 3 year for ICO TBA ("last_avail" as RefDate)
                     (fx_df.sort_values(by='RefDate', ascending=False).groupby('currency_lab', as_index=False)
                      .agg(USDFx=('USDFx', lambda x: x.head(36).mean())).assign(RefDate='last_avail'))])
    # drop currency not included in fx
    avail_curr=fx_df['currency_lab'].unique().tolist()+['USD']
    remov=price_df[~price_df['currency_lab'].isin(avail_curr)].shape[0]
    print(f'- {remov} rows removed because currency FX rate not available')
    display(price_df[~price_df.currency_lab.isin(avail_curr)].currency_lab.value_counts().to_frame())
    price_df=price_df[price_df['currency_lab'].isin(avail_curr)]
    # merge fx_df
    price_df=price_df.merge(fx_df, on=['currency_lab', 'RefDate'], how='left')
    price_df.loc[price_df['currency_lab'] == 'USD', 'USDFx']=1
    approx_fx=price_df[price_df['USDFx'].isna()]
    if approx_fx.shape[0] > 0:
        print('- Taking closest available FX rate for', approx_fx.shape[0], 'rows. Currency:', approx_fx['currency_lab'].unique())
        for i, row in approx_fx.iterrows():
            date=pd.to_datetime(row['RefDate'], format='%Y_%m')
            curr=row['currency_lab']
            match_df=fx_df[(fx_df['currency_lab']==curr) & (fx_df['RefDate'] != 'last_avail')].copy()
            match_df['Date']=pd.to_datetime(match_df['RefDate'], format='%Y_%m')
            fx=match_df[match_df['Date'] == min(match_df['Date'], key=lambda x: (x>date, abs(x-date)))]['USDFx'].values[0]
            price_df.loc[price_df['url']==row['url'], 'USDFx']=fx
    if price_df['USDFx'].isna().sum() > 0:
        print('-', price_df['USDFx'].isna().sum(), 'missing USD Fx rate found. Rows will be removed')
        price_df=price_df[~price_df['USDFx'].isna()]
    print(f'- Price available for {price_df.shape[0]} entries')
    price_df.to_csv(os.path.join(results_folder, '01a_c_ICOmarks_formatted_price_log.csv'), index=False, sep=';')
    print('- Price log saved in', os.path.join(results_folder, '01a_c_ICOmarks_formatted_price_log.csv'))
    # convert to USD
    price_df['PriceUSD']=price_df['currency_unit_num'] / price_df['token_unit_num'] * price_df['USDFx']
    if price_df['PriceUSD'].isna().sum() > 0 or price_df['PriceUSD'].isnull().sum() > 0:
        print('- Warning: Inf or NaN rate found in "PriceUSD"')
    # merge with format df
    format_df=format_df.merge(price_df[['url', 'PriceUSD']], on='url', how='left')
    move_col = format_df.pop('PriceUSD')
    format_df.insert(format_df.columns.get_loc("STOPrice")+1, 'PriceUSD', move_col)
    if format_df.shape[0] != format_df_rows:
        print('########## "format_df" expected rows do not match')


    #### convert PreSalePrice to USD

    format_df.loc[format_df['url']=='https://icomarks.ai/ico/bitether', 'PreSalePrice']='1 ETH = 615 BTT'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/setcoin', 'PreSalePrice']='1 USD = 10 SET'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/sensitrust', 'PreSalePrice']='0.05 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/inmusik', 'PreSalePrice']='0.10 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/swapy-network', 'PreSalePrice']='0.57 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/phuket-holiday-coin', 'PreSalePrice']='1 PHC = 0.125 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/holiday', 'PreSalePrice']='1 NEO = 1500 Holiday Coin'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/immvrse', 'PreSalePrice']='0.20 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/linkercoin', 'PreSalePrice']='1 LNC  = 0.0003 ETH'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/metachessgame', 'PreSalePrice']='1 MTCG = $0.0035'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/floppa-token', 'PreSalePrice']='$5.4 = 1000 FLOP'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/bonzai-technology', 'PreSalePrice']='4166666666 BONZAI = 1 BNB'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/lawrencium-token', 'PreSalePrice']='4000000 XLW = 1BNB'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/renewable-energy-for-all', 'PreSalePrice']='14,000 REFA = 1 BSC'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/zeller', 'PreSalePrice']='1 ZLR = 0.0000124198 BNB'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/metarobotwarrior', 'PreSalePrice']='1MRTW = 0.00005 BNB'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/neutro', 'PreSalePrice']='1 NTO = 0.6 USDT'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/dlife', 'PreSalePrice']='1 DLIFE=$0.028'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/ccecoin1541800979', 'PreSalePrice']='1 CCE = $0.10'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/miningwatchdog-smartchain-token', 'PreSalePrice']='$0.5 = 1 MSC'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/the-sports-bet', 'PreSalePrice']='1 SBET = $0.0035'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/axl-inu', 'PreSalePrice']='$0.00075 = 1AXL'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/porn', 'PreSalePrice']='1 PORN = $ 0.006'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/mtw-games-token', 'PreSalePrice']='0.05 USDT = 1MTW'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/deelance', 'PreSalePrice']='1 DLANCE = 0.025 USDT'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/galobank', 'PreSalePrice']='$0.15 = 1 GALO'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/walletpayin', 'PreSalePrice']='1 WPI  - 0.0000000085 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/x', 'PreSalePrice']='1 X = 0.0000001 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/mosquitos-finance', 'PreSalePrice']='0.05USDT = 1 SUCKR'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/redmemecoin', 'PreSalePrice']='1 BNB = 150000000 RED MEME'


    print('\n** Formatting "PreSalePrice"')
    price_df=format_df[['url', 'Ticker', 'PreSalePrice']].copy()
    price_df=pd.concat([price_df, date_df['RefDate']], axis=1)
    price_df['RefDate']=price_df['RefDate'].dt.year.astype(str)+'_'+price_df['RefDate'].dt.month.astype(str).str.zfill(2)
    price_df['RefDate'].replace('2150_01', 'last_avail', inplace=True)
    price_df['Price']=price_df['PreSalePrice']
    price_df=price_df[price_df['Price']!='']
    price_df['check_split']=price_df.apply(lambda x: len(re.split('==|≈|=|—', x['Price'])), axis=1)
    check_len=price_df.query('check_split > 2')
    if check_len.shape[0] > 0:
        print('- Found rows with wrong entries ("PreSalePriceUSD" will not be evaluated):')
        display(check_len)
        price_df=price_df[~price_df['url'].isin(check_len['url'].values)]
    price_df=pd.concat([price_df, price_df.apply(extract_price, axis=1)], axis=1)
    print('- Errors when parsing "PreSalePrice":')
    display(price_df['error'].value_counts().to_frame())

    price_df.to_csv(os.path.join(results_folder, '01a_c_ICOmarks_formatted_PreSaleprice_error_log.csv'), index=False, sep=';')
    print('- Error log saved in', os.path.join(results_folder, '01a_c_ICOmarks_formatted_PreSaleprice_error_log.csv'))
    price_df=price_df[price_df['error'] == '']
    price_df['currency_lab']=price_df['currency_lab'].str.upper()
    for i, row in CURRENCY_ADJUST.iterrows():
        price_df['currency_lab'].replace(row['original'], row['adjusted'], inplace=True)
    price_df=price_df.merge(cat_list[['url', 'StartDate']], on='url', how='left')  # check if "last_avail" is assigned only on TBA
    if not (price_df[price_df['RefDate']=='']['StartDate'].unique()=='TBA').all():
        print('- Warning: check missing "RefDate", "TBA" as date is automatically assigned and last available FX rate is taken')
    # drop currency not included in fx
    avail_curr=fx_df['currency_lab'].unique().tolist()+['USD']
    remov=price_df[~price_df['currency_lab'].isin(avail_curr)].shape[0]
    print(f'- {remov} rows removed because currency FX rate not available')
    price_df=price_df[price_df['currency_lab'].isin(avail_curr)]
    # merge fx_df
    price_df=price_df.merge(fx_df, on=['currency_lab', 'RefDate'], how='left')
    price_df.loc[price_df['currency_lab'] == 'USD', 'USDFx']=1
    approx_fx=price_df[price_df['USDFx'].isna()]
    if approx_fx.shape[0] > 0:
        print('- Taking closest available FX rate for', approx_fx.shape[0], 'rows. Currency:', approx_fx['currency_lab'].unique())
        for i, row in approx_fx.iterrows():
            date=pd.to_datetime(row['RefDate'], format='%Y_%m')
            curr=row['currency_lab']
            match_df=fx_df[(fx_df['currency_lab']==curr) & (fx_df['RefDate'] != 'last_avail')].copy()
            match_df['Date']=pd.to_datetime(match_df['RefDate'], format='%Y_%m')
            fx=match_df[match_df['Date'] == min(match_df['Date'], key=lambda x: (x>date, abs(x-date)))]['USDFx'].values[0]
            price_df.loc[price_df['url']==row['url'], 'USDFx']=fx
    if price_df['USDFx'].isna().sum() > 0:
        print('-', price_df['USDFx'].isna().sum(), 'missing USD Fx rate found. Rows will be removed')
        price_df=price_df[~price_df['USDFx'].isna()]
    print(f'- Price available for {price_df.shape[0]} entries')
    price_df.to_csv(os.path.join(results_folder, '01a_c_ICOmarks_formatted_PreSaleprice_log.csv'), index=False, sep=';')
    print('- Price log saved in', os.path.join(results_folder, '01a_c_ICOmarks_formatted_PreSaleprice_log.csv'))
    # convert to USD
    price_df['PreSalePriceUSD']=price_df['currency_unit_num'] / price_df['token_unit_num'] * price_df['USDFx']
    if price_df['PreSalePriceUSD'].isna().sum() > 0 or price_df['PreSalePriceUSD'].isnull().sum() > 0:
        print('- Warning: Inf or NaN rate found in "PreSalePriceUSD"')
    # merge with format df
    format_df=format_df.merge(price_df[['url', 'PreSalePriceUSD']], on='url', how='left')
    move_col = format_df.pop('PreSalePriceUSD')
    format_df.insert(format_df.columns.get_loc("PreSalePrice")+1, 'PreSalePriceUSD', move_col)
    if format_df.shape[0] != format_df_rows:
        print('########## "format_df" expected rows do not match')


    #### convert Hard/Soft Cap

    format_df.loc[format_df['url']=='https://icomarks.ai/ico/finebit-token', 'FundSoftCap']='500000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/finebit-token', 'FundHardCap']='7000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/eshop', 'FundSoftCap']='500000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/eshop', 'FundHardCap']='12000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/cardonio', 'FundHardCap']='360000000 CFGT'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/cardonio', 'FundSoftCap']='220000000 CFGT'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/masternode-invest', 'FundHardCap']='6500000 MS'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/united-farmers-x', 'FundHardCap']='205000000 UFX'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/123swap', 'FundHardCap']='40000000 123'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/konios-project', 'FundHardCap']='29000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/eddmate-token', 'FundHardCap']=''
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/smrt', 'FundHardCap']='53000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/yaffa', 'FundHardCap']='160000000 ENFT'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/ethichub', 'FundHardCap']='2,000 ETH'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/aurora', 'FundHardCap']='5675200 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/levelnet', 'FundHardCap']='12000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/weekend-millionaires-club', 'FundHardCap']='2000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/ebankx', 'FundHardCap']='78500000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/baby-musk-coin', 'FundHardCap']='1544000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/x3-protocol', 'FundHardCap']='28890000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/mira', 'FundHardCap']='20000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/gameflip', 'FundHardCap']='12000 ETH'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/beuthereum', 'FundHardCap']='8000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/altair-vr', 'FundHardCap']='10,000 ETH'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/kiwilemon', 'FundHardCap']='USD 20000000'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/quanta-networks', 'FundHardCap']='300000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/iziing', 'FundHardCap']='30000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/stellerro', 'FundHardCap']='5000000 EUR'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/tellefinance', 'FundHardCap']='3500000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/digital-euro', 'FundHardCap']='100000000 EUR'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/coinplace', 'FundHardCap']='20000 ETH'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/fit-token', 'FundHardCap']='67000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/hut34-project', 'FundHardCap']='60000 ETH'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/orfeus-network', 'FundHardCap']='50000 BNB'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/giga-giving', 'FundHardCap']='12000000 GC'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/coinseed', 'FundHardCap']='1500000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/hycon', 'FundSoftCap']='13900000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/gelios', 'FundSoftCap']='1000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/ethichub', 'FundSoftCap']='1000 ETH'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/levelnet', 'FundSoftCap']='1500000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/ebankx', 'FundSoftCap']='1000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/baby-musk-coin', 'FundSoftCap']='1544000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/x3-protocol', 'FundSoftCap']='$28890000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/mira', 'FundSoftCap']='5000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/altair-vr', 'FundSoftCap']='500 ETH'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/healthureum', 'FundSoftCap']='15000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/kiwilemon', 'FundSoftCap']='5000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/quanta-networks', 'FundSoftCap']='20000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/iziing', 'FundSoftCap']='5000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/stellerro', 'FundSoftCap']='500000 EUR'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/digital-euro', 'FundSoftCap']='10000000 EUR'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/orfeus-network', 'FundSoftCap']='1000 BNB'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/libra-ecosystem', 'FundSoftCap']=''
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/libra-ecosystem', 'FundHardCap']=''
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/armacoin', 'FundHardCap']='2380000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/global-innovative-solutions', 'FundHardCap']='2100000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/aliencloud', 'FundHardCap']='600000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/citicash', 'FundHardCap']='19500000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/copernic', 'FundHardCap']='3200000 PLN'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/edenbest', 'FundHardCap']='16875 ETH'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/edenbest', 'FundSoftCap']='11812 ETH'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/arealeum', 'FundHardCap']='73392857 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/bolton-coin-bfcl', 'FundHardCap']='2198960 ETH'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/chainpals', 'FundHardCap']='2425000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/sleekplay', 'FundHardCap']='26000 ETH'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/x3-protocol', 'FundSoftCap']='28890000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/citicash', 'FundSoftCap']='5000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/escotoken', 'FundSoftCap']='16000 ETH'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/posschain', 'FundSoftCap']='1725000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/brickken', 'FundSoftCap']='500000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/Metacoms', 'FundSoftCap']='500000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/flexion', 'FundHardCap']='5400000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/flexion', 'FundSoftCap']='900000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/flexion', 'FundHardCap']='2000 BNB'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/fanchain', 'FundHardCap']='330000000 FANZ'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/b-money', 'FundHardCap']='788190 BMNY'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/micropad', 'FundHardCap']='850000 MICROPAD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/custodi', 'FundHardCap']='35000000 Custodi Cash Token'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/fanchain', 'FundSoftCap']='50000000 FANZ'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/b-money', 'FundSoftCap']='118220 BMNY'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/micropad', 'FundSoftCap']='425000 MICROPAD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/cakepad', 'FundSoftCap']='80000000 Cakepad'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/coingrid', 'FundHardCap']='10000000 CGT'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/coingrid', 'FundSoftCap']='70000000 CGT'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/cdrx', 'FundHardCap']='100000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/cdrx', 'FundSoftCap']='5000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/coinchase', 'FundHardCap']='41900 ETH'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/coinchase', 'FundSoftCap']='400 ETH'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/dago-mining', 'FundHardCap']='20000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/dago-mining', 'FundSoftCap']='4000000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/mosquitos-finance', 'FundSoftCap']='15000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/mosquitos-finance', 'FundHardCap']='75000 USD'
    format_df.loc[format_df['url']=='https://icomarks.ai/ico/vanillaplay', 'FundHardCap']='15,000,000 USD'


    print('\n** Formatting "FundHardCap" and "FundSoftCap"')
    date_df=format_df[[col for col in format_df.columns if 'date' in col.lower()]].replace('TBA', '').replace(np.nan, '').fillna(pd.to_datetime('1 Jan 2150'))
    date_df['RefDate']=date_df.apply(lambda x: min([y for y in x if type(y) != str]), axis=1)
    # date_df.replace(pd.to_datetime('1 Jan 2150'), np.nan, inplace=True)
    price_df=format_df[['url', 'Ticker', 'FundHardCap', 'FundSoftCap']].copy()
    price_df=pd.concat([price_df, date_df['RefDate']], axis=1)
    price_df['RefDate']=price_df['RefDate'].dt.year.astype(str)+'_'+price_df['RefDate'].dt.month.astype(str).str.zfill(2)
    price_df['RefDate'].replace('2150_01', 'last_avail', inplace=True)
    price_df=pd.concat([price_df[['url', 'Ticker', 'RefDate', 'FundHardCap']].assign(type='FundHardCap').rename(columns={'FundHardCap': 'Price'}),
              price_df[['url', 'Ticker', 'RefDate', 'FundSoftCap']].assign(type='FundSoftCap').rename(columns={'FundSoftCap': 'Price'})])
    price_df=price_df[price_df['Price']!='']
    price_df['check_split']=price_df.apply(lambda x: len(re.split('==|≈|=|—', x['Price'])), axis=1)
    check_len=price_df.query('check_split > 2')
    if check_len.shape[0] > 0:
        print('- Found rows with wrong entries ("FundHardCap" or "FundSoftCap" will not be evaluated):')
        display(check_len)
        price_df=price_df[~price_df['url'].isin(check_len['url'].values)]
    price_df=pd.concat([price_df, price_df.apply(extract_price, axis=1)], axis=1)
    # try to recover missing currency from ICOPrice, IEOPrice, STOPrice
    priceusd_df=pd.read_csv(os.path.join(results_folder, '01a_c_ICOmarks_formatted_price_log.csv'), sep=';')
    priceusd_df=priceusd_df[['url', 'currency_lab']].rename(columns={'currency_lab': 'currency_lab_PriceUSD'})
    priceusd_df=pd.concat([priceusd_df.assign(type='FundHardCap'), priceusd_df.assign(type='FundSoftCap')])
    row_exp=price_df.shape[0]
    price_df=price_df.merge(priceusd_df, on=['url', 'type'], how='left')
    price_df=price_df.merge(format_df[['url', 'PriceUSD', 'FundRaisedUSD']], on='url', how='left')
    if price_df.shape[0] != row_exp:
        print('###### warning: rows in "price_df" mismatch after join with "priceusd_df"')
    # try to recover Price without currency label by currency_lab from PriceUSD
    recov_df=price_df[(price_df['error']=='currency range error missing label-missing currency unit numeric') & (~price_df['currency_lab_PriceUSD'].isna())].copy()
    if len(recov_df) > 0:
        recov_df['Price']=recov_df['Price']+' '+recov_df['currency_lab_PriceUSD']
        recov_df=recov_df[['url', 'Ticker', 'RefDate', 'Price', 'type', 'currency_lab_PriceUSD', 'PriceUSD', 'FundRaisedUSD']]
        recov_df=pd.concat([recov_df, recov_df.apply(extract_price, axis=1)], axis=1)
        recov_df=recov_df[recov_df['error']=='']
        recov_df['warning']='currency recovered by PriceUSD'
        price_df=price_df[~price_df.index.isin(recov_df.index)]
        price_df=pd.concat([price_df, recov_df])
    if price_df.shape[0] != row_exp:
        print('###### warning: rows in "price_df" mismatch after join with "recov_df"')
    price_df.loc[(price_df['error'] == 'missing currency unit numeric') & (price_df['currency_part'] == ''), 'error']=''
    print('- Errors when parsing "FundHardCap" and "FundSoftCap":')
    display(price_df['error'].value_counts().to_frame())

    price_df.to_csv(os.path.join(results_folder, '01a_c_ICOmarks_formatted_HardSoftCap_error_log.csv'), index=False, sep=';')
    print('- Error log saved in', os.path.join(results_folder, '01a_c_ICOmarks_formatted_HardSoftCap_error_log.csv'), end='\n')
    price_df=price_df[price_df['error'] == '']
    price_df['currency_lab']=price_df['currency_lab'].str.upper()
    for i, row in CURRENCY_ADJUST.iterrows():
        price_df['currency_lab'].replace(row['original'], row['adjusted'], inplace=True)
    price_df=price_df.merge(cat_list[['url', 'StartDate']], on='url', how='left')  # check if "last_avail" is assigned only on TBA
    if not (price_df[price_df['RefDate']=='']['StartDate'].unique()=='TBA').all():
        print('- Warning: check missing "RefDate", "TBA" as date is automatically assigned and last available FX rate is taken')
    ## evaluate Hard/SoftCap value in USD
    only_token_df=price_df[(price_df['token_part'] != '') & (price_df['currency_part'] == '')].copy()
    only_currency_df=price_df[(price_df['token_part'] == '') & (price_df['currency_part'] != '')].copy()
    both_df=price_df[(price_df['token_part'] != '') & (price_df['currency_part'] != '')].copy()
    ## only_token_df
    if len(only_token_df) + len(only_currency_df) +len(both_df) != len(price_df):
        print('- Warning: missing rows when splitting into "only_token_df", "only_currency_df" and "both_df"')
    only_token_df['CapUSD']=only_token_df['token_unit_num']*only_token_df['PriceUSD']
    if only_token_df.CapUSD.isna().sum() > 0:
        print(f'- {only_token_df.CapUSD.isna().sum()} rows in "only_token_df" (with Hard/Soft Cap in tokens) skipped because of missing "PriceUSD"')
    only_token_df=only_token_df[~only_token_df.CapUSD.isna()]
    print(f'- {only_token_df.shape[0]} rows remaing in "only_token_df" (with Hard/Soft Cap in tokens)')
    if only_token_df['CapUSD'].isna().sum() > 0 or only_token_df['CapUSD'].isnull().sum() > 0:
        print('- Warning: Inf or NaN rate found in "CapUSD" in "only_token_df"')
    ## only_currency_df
    # drop currency not included in fx
    avail_curr=fx_df['currency_lab'].unique().tolist()+['USD']
    remov=only_currency_df[~only_currency_df['currency_lab'].isin(avail_curr)].shape[0]
    if remov > 0:
        print(f'- {remov} rows in "only_currency_df" (with Hard/Soft Cap in currency) skipped because FX rate not available')
        display(price_df[~price_df.currency_lab.isin(avail_curr)].currency_lab.value_counts().to_frame())
    only_currency_df=only_currency_df[only_currency_df['currency_lab'].isin(avail_curr)]
    # merge fx_df
    only_currency_df=only_currency_df.merge(fx_df, on=['currency_lab', 'RefDate'], how='left')
    only_currency_df.loc[only_currency_df['currency_lab'] == 'USD', 'USDFx']=1
    approx_fx=only_currency_df[only_currency_df['USDFx'].isna()]
    if approx_fx.shape[0] > 0:
        print('- Taking closest available FX rate for', approx_fx.shape[0], 'rows. Currency:', approx_fx['currency_lab'].unique())
        for i, row in approx_fx.iterrows():
            date=pd.to_datetime(row['RefDate'], format='%Y_%m')
            curr=row['currency_lab']
            match_df=fx_df[(fx_df['currency_lab']==curr) & (fx_df['RefDate'] != 'last_avail')].copy()
            match_df['Date']=pd.to_datetime(match_df['RefDate'], format='%Y_%m')
            fx=match_df[match_df['Date'] == min(match_df['Date'], key=lambda x: (x>date, abs(x-date)))]['USDFx'].values[0]
            only_currency_df.loc[only_currency_df['url']==row['url'], 'USDFx']=fx
    if only_currency_df['USDFx'].isna().sum() > 0:
        print('-', only_currency_df['USDFx'].isna().sum(), 'missing USD Fx rate found. Rows will be removed')
        only_currency_df=only_currency_df[~only_currency_df['USDFx'].isna()]
    print(f'- {only_currency_df.shape[0]} rows remaing in "only_currency_df" (with Hard/Soft Cap in currency)')
    # convert to USD
    only_currency_df['CapUSD']=only_currency_df['currency_unit_num'] * only_currency_df['USDFx']
    if only_currency_df['CapUSD'].isna().sum() > 0 or only_currency_df['CapUSD'].isnull().sum() > 0:
        print('- Warning: Inf or NaN rate found in "CapUSD" in "only_currency_df"')
    ## both_df
    if len(both_df) > 0:
        print(f'- All {len(both_df)} rows in "both_df" (with Hard/Soft Cap in currency AND token) skipped because of mismatch')

    price_df=pd.concat([only_currency_df, only_token_df])
    print(f'\n- Hard/Soft Cap available for {price_df.shape[0]} entries')
    price_df.to_csv(os.path.join(results_folder, '01a_c_ICOmarks_formatted_HardSoftCap_log.csv'), index=False, sep=';')
    print('- Price log saved in', os.path.join(results_folder, '01a_c_ICOmarks_formatted_HardSoftCap_log.csv'))
    # merge with format df
    rows=price_df.shape[0]
    price_df=price_df[['url', 'type', 'CapUSD']].pivot(index='url',columns='type',values='CapUSD').reset_index().rename(columns={'FundHardCap': 'FundHardCapUSD', 'FundSoftCap': 'FundSoftCapUSD'})
    if len(price_df) * 2 - price_df.drop(columns='url').isna().sum().sum() != rows:
        print('###### warning: spreading price_df generated unexpected missing')
    format_df=format_df.merge(price_df[['url', 'FundHardCapUSD', 'FundSoftCapUSD']], on='url', how='left')
    move_col = format_df.pop('FundHardCapUSD')
    format_df.insert(format_df.columns.get_loc("FundHardCap")+1, 'FundHardCapUSD', move_col)
    move_col = format_df.pop('FundSoftCapUSD')
    format_df.insert(format_df.columns.get_loc("FundSoftCap")+1, 'FundSoftCapUSD', move_col)
    if format_df.shape[0] != format_df_rows:
        print('########## "format_df" expected rows do not match')
        
        
    #### format Platform
    
    print('\n** Formatting "Platform"')
    unique_platform=[]
    for v in format_df['Platform'].drop_duplicates().map(extract_platform).values:
        unique_platform.extend(v)
    print('- Unique platform found:', len(np.unique(unique_platform)))
    pd.DataFrame({'Platform': np.unique(unique_platform)}).to_csv(os.path.join(results_folder, '01a_d_ICOmarks_formatted_Platform_unique.csv'), index=False, sep=';')
    print('- Unique list saved in', os.path.join(results_folder, '01a_d_ICOmarks_formatted_Platform_unique.csv'))    
    format_df['Platform']=format_df['Platform'].drop_duplicates().map(extract_platform)


    #### format TokenAvailForSale and TokenTotSupply

    print('\n** Formatting "TokenAvailForSale" and "TokenTotSupply"')
    token_df=format_df[['url', 'Ticker', 'TokenAvailForSale', 'TokenTotSupply']]
    token_df=pd.concat([token_df, token_df.apply(extract_TokenAvailForSale, axis=1)], axis=1)
    token_df['num_part_suppl']=np.where((~token_df['token_perc'].isna()) & (~token_df['num_part'].isna()), token_df['num_part'] / token_df['token_perc'] * 100, token_df['num_part_suppl'])
    token_df['num_part_suppl']=round(token_df['num_part_suppl'])
    token_df['num_part_suppl']=np.where((token_df['num_part']>0) & (token_df['num_part_suppl']==0), token_df['num_part'], token_df['num_part_suppl'])

    print('- Errors when parsing "TokenAvailForSale":')
    display(token_df['error'].value_counts().to_frame())
    token_df.to_csv(os.path.join(results_folder, '01a_e_ICOmarks_formatted_TokenAvailForSale_error_log.csv'), index=False, sep=';')
    print('- Error log saved in', os.path.join(results_folder, '01a_e_ICOmarks_formatted_TokenAvailForSale_error_log.csv'))

    format_df=format_df.merge(token_df[['url', 'num_part', 'num_part_suppl']], on='url', how='left')
    move_col = format_df.pop('num_part')
    format_df.insert(format_df.columns.get_loc("TokenTotSupply")+1, 'num_part', move_col)
    move_col = format_df.pop('num_part_suppl')
    format_df.insert(format_df.columns.get_loc("num_part")+1, 'num_part_suppl', move_col)
    format_df=format_df.rename(columns={'TokenAvailForSale': 'TokenAvailForSaleOriginal', 'TokenTotSupply': 'TokenTotSupplyOriginal',
                                       'num_part': 'TokenAvailForSale', 'num_part_suppl': 'TokenTotSupply'})


    #### format AcceptedCurr

    print('\n** Formatting "AcceptedCurr"')
    unique_curr=[]
    for v in format_df['AcceptedCurr'].drop_duplicates().map(extract_AcceptedCurr).values:
        unique_curr.extend(v)
    print('- Unique AcceptedCurr found:', len(np.unique(unique_curr)))
    pd.DataFrame({'AcceptedCurr': np.unique(unique_curr)}).to_csv(os.path.join(results_folder, '01a_f_ICOmarks_formatted_AcceptedCurr_unique.csv'), index=False, sep=';')
    print('- Unique list saved in', os.path.join(results_folder, '01a_f_ICOmarks_formatted_AcceptedCurr_unique.csv'))    
    format_df['AcceptedCurr']=format_df['AcceptedCurr'].map(extract_AcceptedCurr)

    return format_df


def scrape_info_cryptototem(url):

    page = requests.get(url)
    soup = BeautifulSoup(page.content, 'html.parser')


    # get website
    website=''
    website_err=''
    try:
        tt=convert(soup.find("td", class_="thumbs-holder").find('span', class_='decr'))
        if tt['#text'] == 'Website':
            crypted=tt['@data-u']
            lcls = locals()
            exec('dd='+crypted, globals(), lcls )
            dd = lcls["dd"]
            website=decrypt_CryptoTotem(dd).replace('?utm_source=cryptototem', '')
    except Exception as err:
        website_err=err

    # get whitepaper
    whitepaper_url=''
    whitepaper_err=''
    if soup.find("td", string="Whitepaper") is not None:
        try:
            tt=convert(soup.find("td", string="Whitepaper").find_next_sibling("td").span)
            if tt['#text'] == 'Open':
                crypted=tt['@data-u']
                lcls = locals()
                exec('dd='+crypted, globals(), lcls )
                dd = lcls["dd"]
                whitepaper_url=decrypt_CryptoTotem(dd)
        except Exception as err:
            whitepaper_err=err

    else:
        whitepaper_err='NOT AVAILABLE'

    # get Bounty program
    bounty_dummy=0
    if soup.find("div", string="Bounty program") is not None:
        bounty_dummy=1

    # get MVP
    MVP_dummy=0
    if soup.find("h2", string="MVP") is not None:
        MVP_dummy=1

    # get Overview
    overview_err=''
    try:
        tt=convert(soup.find("td", class_="overview-td"))['table'][0]['tr']
        overview_block=pd.DataFrame()
        for el in tt:
            add_row=[x['#text'] for x in el['td']]
            overview_block=pd.concat([overview_block, pd.DataFrame(add_row, index=['Field', 'Value']).T])
    except Exception as err:
        overview_err=err
        overview_block=pd.DataFrame()

    # check if price chart is available (in case, use coingecko API  https://rapidapi.com/collection/coinmarketcap-api)
    price_avail=int(soup.find("div", class_="chart-holder") is not None)

    # get status and tags (ICO, IEO, ...)
    tt=convert(soup.find("h1", class_="ico-title").parent)['div']
    try:
        last_update=tt[0]['#text'].split('Last updated:')[-1].strip()
    except:
        last_update=None
    try:
        tag=[x['#text'] for x in tt[0]['div']]
    except:
        tag=None
    try:
        status=[x['div'][0]['@class'][-1] for x in tt[2]['div']]
    except:
        status=None

    # get description
    description=''
    description_err=''
    try:
        for el in soup.find_all("div", class_="align-left"):
            if el.find('h2') is not None:
                if "What is" in str(el.find('h2')):
                    description=convert(el)['#text']
    except Exception as err:
        description_err=err

    # get Info
    info_err=''
    info_block=pd.DataFrame()
    try:
        for child in soup.find("table", class_="ico-main-table token-info-table").children:
            for td_column in child:
                h2=''
                wait_for_value=True
                for td in td_column:
                    if isinstance(td, Tag):
                        tag_type=td.prettify().split('\n')[0]
                        if tag_type == '<h2>':
                            main_category=td.text
                        elif tag_type == '<strong>':
                            label=td.text
                            wait_for_value=True
                        elif tag_type == '<address>':
                            label='Office address'
                            value=td.text.split('Office address: ')[-1]
                            wait_for_value=False
                        elif tag_type == '<br/>':
                            continue
                    else:   # NavigableString
                        value=td.text.strip()
                        wait_for_value=False
                    if not wait_for_value:
                        info_block=pd.concat([info_block, pd.DataFrame({'category': main_category, 'label': label,
                                                                        'value': value}, index=[0])])
    except Exception as err:
        info_err=err

    # get Team and Advisors
    team_err=''
    team_block=pd.DataFrame()
    try:
        for label, cls  in zip(['Team', 'Advisor'], ['team-members align-center', 'advisors align-center']):
            tt=soup.find("div", class_=cls)
            if tt is not None:
                tt=convert(tt)
                for el in tt['div']:
                    try:
                        pers_name=el['div'][0]['#text']
                    except:
                        pers_name=None
                    try:
                        pers_role = el['div'][1]['#text']
                    except:
                        pers_role=None
                    try:
                        pers_url=[]
                        for el1 in el['span']:
                            crypted=el1['@data-u']
                            lcls = locals()
                            exec('dd='+crypted, globals(), lcls )
                            dd = lcls["dd"]
                            pers_url.append(decrypt_CryptoTotem(dd).replace('\\r', ''))
                    except:
                        pers_url=[]
                    team_block=pd.concat([team_block, pd.DataFrame({'Member': label, 'Name': pers_name, 'Role': pers_role,
                                                                    'Links': [pers_url]}, index=[0])])
    except Exception as err:
        team_err=err

    # get milestones
    milestone_err=''
    milestone_block=pd.DataFrame()
    try:
        tt=soup.find("div", class_="milestones")
        if tt:
            tt=convert(tt.find("div", class_="box"))
            for i, el in enumerate(tt['div']):
                try:
                    ref=list(set(el['div'][1].keys()) - set(['@class', '#text', 'div']))[0]  # can be 'p' or 'li' or 'ul'
                    milestone=el['div'][1][ref][0]['#text']
                except:
                    milestone=''
                try:
                    date=el['div'][1]['div'][1]['#text']
                except:
                    date=''
                milestone_block=pd.concat([milestone_block, pd.DataFrame({'date': date, 'milestone': milestone}, index=[i])])
    except Exception as err:
        milestone_err=err

    # get social info
    social_err=''
    social_info=pd.DataFrame()
    try:
        tt=convert(soup.find("div", class_="soc-urls"))
        if len(tt) > 0:
            if 'a' in tt:
                for el in tt['a']:
                    try:
                        social_name=el['@title'].split(':')[0]
                    except:
                        social_name=''
                    try:
                        social_url=el['@href']
                    except:
                        social_url=''
                    social_info=pd.concat([social_info, pd.DataFrame({'Social': social_name, 'Link': social_url}, index=[0])])
            else:
                social_err='only website'
    except Exception as err:
        social_err=err


    add_row=pd.DataFrame({'url': url,
                          'last_update': last_update,
                          'tag': [tag],
                          'status': [status],
                          'website': website,
                          'website_err': website_err,
                          'whitepaper_url': whitepaper_url,
                          'whitepaper_err': whitepaper_err,
                          'bounty_dummy': bounty_dummy,
                          'MVP_dummy': MVP_dummy,
                          'overview_block': [overview_block],
                          'overview_err': overview_err,
                          'price_series_avail': price_avail,
                          'description': description,
                          'description_err': description_err,
                          'info_block': [info_block],
                          'info_err': info_err,
                          'milestone_block': [milestone_block],
                          'milestone_err': milestone_err,
                          'team_block': [team_block],
                          'team_err': team_err,
                          'social_info': [social_info],
                          'social_err': social_err}, index=[0])

    return add_row


def set_global_logging_level(level=logging.ERROR, prefices=[""]):
    """
    Override logging levels of different modules based on their name as a prefix.
    It needs to be invoked after the modules have been loaded so that their loggers have been initialized.

    Args:
        - level: desired level. e.g. logging.INFO. Optional. Default is logging.ERROR
        - prefices: list of one or more str prefices to match (e.g. ["transformers", "torch"]). Optional.
          Default is `[""]` to match all active loggers.
          The match is a case-sensitive `module_name.startswith(prefix)`
    """
    prefix_re = re.compile(fr'^(?:{ "|".join(prefices) })')
    for name in logging.root.manager.loggerDict:
        if re.match(prefix_re, name):
            logging.getLogger(name).setLevel(level)
            
            
def query(payload, api_url, headers):
    
    try:
        data = json.dumps(payload)
        response = requests.request("POST", api_url, headers=headers, data=data)
        out = json.loads(response.content.decode("utf-8"))
    except Exception as e:
        out = {'error': e}
    return out


def format_results(query_output):
    res=pd.DataFrame()
    for index, x in enumerate(query_output):
        row=pd.DataFrame()
        for y in x:
            row=pd.concat([row, pd.DataFrame({y['label']: [y['score']]})], axis=1)
        row.insert(0, 'max', row.idxmax('columns'))
        res=pd.concat([res, row])
    return res

def format_results_local(query_output, pred_classes):
    query_output = query_output.detach().numpy()
    classes = [pred_classes[x] for x in sorted(pred_classes.keys())]
    res=pd.DataFrame()
    for index, x in enumerate(query_output):
        row=pd.DataFrame(x.reshape(-1, len(x)), columns = classes)
        row.insert(0, 'max', classes[np.argmax(x)])
        res=pd.concat([res, row])
    return res

def chunk_sentence(sentence_list=[], tokenizer=None, max_length=0, rolling_window_perc=0.7):
    
    '''
    Split each sentence in sentence_list into chunks of max_length-6 with a rolling window of
    int(rolling_window_perc*max_length) so as to have a (1-rolling_window_perc)*max_length words' overlap
    between consecutive chunks. Each chunk is evaluated according to the corresponding tokens (may be more than
    total words in sentence) encoded by "tokenizer". Then tokens are decoded back to string so as to be ready
    to be processed by HuggingFace API, avoiding "Input is too long" error in the query.
    
    Args:
        - sentence_list: (list) list of sentence to be splitted
        - tokenizer: tokenizer such as BertTokenizer.from_pretrained()
        - max_length: (int) maximum number of token that can be processed by model
        - rolling_window_perc: (float in [0, 1]) percentage of "max_length" to be used as step for the rolling window.
                                (1-rolling_window_perc) will result into the number of words that overlap in each chunk.
    '''
    
    set_global_logging_level(logging.ERROR)   # silence warning for exceeding max_length in tokenizer

    reference_index=[]
    sentence_chunks=[]
    for index, txt in sentence_list.items():

        print('Processing ' + str(index + 1) + ' / ' + str(len(sentence_list)), end = '\r')
        
        # tokenize
        tk=tokenizer.encode_plus(txt, add_special_tokens=False, return_tensors='pt')
        tokens=tk['input_ids'][0]

        # split into overlapping chunks
        max_ind=len(tokens)-1
        window_size=max_length-6
        window_step=int(rolling_window_perc*max_length)
        token_chunks_ind=[]
        i=0
        while True:
            cnk=list(range(i*window_step, min([i*window_step+window_size, len(tokens)])))
            token_chunks_ind.append(cnk)
            i+=1
            if max(cnk) == max_ind:
                break
        if np.in1d(np.array([[x[0], x[-1]] for x in token_chunks_ind]).ravel(), [0, max_ind]).sum() != 2: # check skipped start/end of sentence
            print(f'- Index {index}: token chunk index do not contain start and/or end of sentence')

        # decode back to sentence
        for ind in token_chunks_ind:
            sentence_chunks.append(tokenizer.decode(tokens[ind]))

        # store reference index to map each chunk to corresponding original sentence
        reference_index.extend([str(index).zfill(6)+'_'+str(i) for i in range(len(token_chunks_ind))]) # index_ChunkNum
        
    set_global_logging_level(logging.WARNING)
    if len(reference_index) != len(sentence_chunks):
        print('\n##### Warning: "reference_index" has different length from "sentence_chunks"')
    
    return reference_index, sentence_chunks


# Definition of ESGify class because of custom,sentence-transformers like, mean pooling function and classifier head
class ESGify(MPNetPreTrainedModel):
    """Model for Classification ESG risks from text."""

    def __init__(self,config): #tuning only the head
        """
        """
        super().__init__(config)
        # Instantiate Parts of model
        self.mpnet = MPNetModel(config,add_pooling_layer=False)
        self.id2label =  config.id2label
        self.label2id =  config.label2id
        self.classifier = torch.nn.Sequential(OrderedDict([('norm',torch.nn.BatchNorm1d(768)),
                                                ('linear',torch.nn.Linear(768,512)),
                                                ('act',torch.nn.ReLU()),
                                                ('batch_n',torch.nn.BatchNorm1d(512)),
                                                ('drop_class', torch.nn.Dropout(0.2)),
                                                ('class_l',torch.nn.Linear(512 ,47))]))
        
    def forward(self, input_ids, attention_mask):
         # Feed input to mpnet model
        outputs = self.mpnet(input_ids=input_ids,
                             attention_mask=attention_mask)
         
        # mean pooling dataset and eed input to classifier to compute logits
        logits = self.classifier(ESGify.mean_pooling(outputs['last_hidden_state'],attention_mask))
         
        # apply sigmoid
        softmax = torch.nn.functional.softmax(logits, dim=1)
#         logits  = 1.0 / (1.0 + torch.exp(-logits))
        return softmax

    # Mean Pooling - Take attention mask into account for correct averaging
    @staticmethod
    def mean_pooling(model_output, attention_mask):
            token_embeddings = model_output #First element of model_output contains all token embeddings
            input_mask_expanded = attention_mask.unsqueeze(-1).expand(token_embeddings.size()).float()
            return torch.sum(token_embeddings * input_mask_expanded, 1) / torch.clamp(input_mask_expanded.sum(1), min=1e-9)
        
        
def sentence_classification(df_text, model_ID_list=[''], rolling_window_perc=0.7, query_batch_size=50, custom_model_ID=[''],
                            split_reload=False, query_reload=False, cache_dir='', api_url='', tokens=[],
                            checkpoint_folder='', sentiment_folder='', uniqueID='url', uniqueID_root='https://icomarks.com/ico/'):
    
    '''
    Evaluate text classification on sentences and returns class probabilities.
    
    Args:
        - model_ID_list: (list of str) HuggingFace model ID. E.g. 'nbroad/ESG-BERT'
        - rolling_window_perc: (float in [0, 1]) see chunk_sentence()
        - query_batch_size: (int) batch size of sentences to be sent to API
        - custom_model_ID: (list of str) HuggingFace model ID for which local evaluation is needed. E.g. 'ai-lab/ESGify'
        - split_reload: (bool) if True reload the splitting of sentences into chunks by chunk_sentence()
        - query_reload: (bool) if True reload the query result of a single chunk
        - cache_dir: (str) path for caching HuggingFace model (model will be downloaded if not found in the folder)
        - api_url: (str) url for HuggingFace API. Will be merged with model_ID
        - tokens: (str, list) list of token(s) to be used for the API. If multiple values in a list, token will be rotated.
        - uniqueID: (str) unique ID column name
    '''

    if type(model_ID_list) != list:
        raise ValueError('"model_ID_list" must be a list of string')
        
    if type(tokens) == str:
        tokens = [tokens]
    elif type(tokens) != list:
        raise ValueError('"tokens" must be a string or list of string')
    
    output={}
    for model_ID in model_ID_list:
        
        print('\n\n'+'#'*(70+len(model_ID)+4))
        print('#'+' '*35, model_ID, ' '*35+'#')
        print('#'*(70+len(model_ID)+4),'\n\n')
        model_lab=model_ID.replace('/','_')
        work_query_batch_size = query_batch_size
        
        query_folder = os.path.join(sentiment_folder, model_lab)
        if not os.path.exists(query_folder):
            os.makedirs(query_folder)
    
        ### define model and tokenizer
        if model_ID.startswith('ESGBERT/'):
            model = AutoModelForSequenceClassification.from_pretrained(model_ID, cache_dir=cache_dir)
            tokenizer = AutoTokenizer.from_pretrained(model_ID, max_len=512, do_lower_case=True, cache_dir=cache_dir)
        elif model_ID not in custom_model_ID: 
            model = BertForSequenceClassification.from_pretrained(model_ID, cache_dir=cache_dir)
            tokenizer = BertTokenizer.from_pretrained(model_ID, do_lower_case=True, cache_dir=cache_dir)
        elif model_ID == 'ai-lab/ESGify':
            model = ESGify.from_pretrained('ai-lab/ESGify', cache_dir=cache_dir)
            tokenizer = AutoTokenizer.from_pretrained('ai-lab/ESGify', do_lower_case=True, cache_dir=cache_dir)
            model.config.max_position_embeddings=512
            work_query_batch_size = 20
            print('\n"query_batch_size" switched to:', work_query_batch_size)

        ### get prediction classes, special tokens and maximum sentence (in tokens) length
        # https://huggingface.co/transformers/v4.0.1/model_doc/bert.html?highlight=do_lower_case#berttokenizer

        pred_classes=model.config.id2label
        print('Prediction classes:')
        display(pd.DataFrame(pred_classes.values(), index=pred_classes.keys(), columns=['Class']))

        max_length = model.config.max_position_embeddings
        print('\nMaximum tokens allowed:', max_length)

        # get input_ids coding for "PAD". CLS will always be at the beginning, SEP at the end
        tk = tokenizer("[PAD]", truncation=False)
        token_CLS = tk['input_ids'][0]     # beginning of sentence, CLS for classification
        token_PAD = tk['input_ids'][1]
        token_SEP = tk['input_ids'][2]
        tk=tokenizer("[UNK]", truncation=False)
        token_UNK = tk['input_ids'][1]
        print('\nSpecial Tokens:\n[PAD]:', token_PAD , '\n[CLS]:', token_CLS, '\n[SEP]:', token_SEP, '\n[UNK]:', token_UNK)

        ### split the sentences into chunks
        print('\n\n-- Split sentences into chunks:\n')
        split_path=os.path.join(sentiment_folder, '00_sentence_split_'+model_lab+'.pkl')
        if not split_reload or not os.path.exists(split_path):
            start=timer()
            reference_index, sentence_chunks = chunk_sentence(sentence_list=df_text['text_clean'],
                                                              tokenizer=tokenizer, max_length=max_length, rolling_window_perc=0.7)
            tot_time=str(datetime.timedelta(seconds=round(timer()-start)))
            joblib.dump({'reference_index': reference_index, 'sentence_chunks': sentence_chunks, 'tot_time': tot_time},
                        split_path, compress=('lzma', 3))
            print('\nDone in ', tot_time)
            print('Data saved in', split_path)
        else:
            rr=joblib.load(split_path)
            reference_index=rr['reference_index']
            sentence_chunks=rr['sentence_chunks']
            tot_time=rr['tot_time']
            print(f'Reloaded (evaluated in {tot_time})')
        print(f'\nTotal sentences: {len(df_text)}')
        print(f'Total chunked sentences: {len(reference_index)}')


        ### query from API
        if model_ID not in custom_model_ID:
            print('\n\n-- Query from API:')
        else:
            print('\n\n-- Query from', colored('LOCALE', 'black', 'on_light_grey')+':')
        # split chunks for query and create mapping for saving correct url->pkl_lab
        chunk_ind_mapping = pd.DataFrame({'ref_index': reference_index})
        chunk_ind_mapping[['index', 'Chunk']] = chunk_ind_mapping['ref_index'].str.split('_', expand=True)
        chunk_ind_mapping['index']=chunk_ind_mapping['index'].astype(int)
        chunk_ind_mapping = chunk_ind_mapping.merge(df_text[[uniqueID]].reset_index(), on='index', how='left')
        chunk_ind_mapping['chunk_name']=chunk_ind_mapping[uniqueID].str.replace(uniqueID_root, '', regex=False)
        chunk_ind_mapping['pkl_lab']=''
        if sum(chunk_ind_mapping[uniqueID].isna()):
            missing_ind = chunk_ind_mapping[chunk_ind_mapping[uniqueID].isna()]['index'].unique()
            raise ValueError(f'- empty "{uniqueID}" when matching chunks indexes in "chunk_ind_mapping". Reference index: {missing_ind}')
        chunk_ind=[]
        chunk_ind_label=[]
        for i in chunk_ind_mapping['index'].unique():
            tt = chunk_ind_mapping[chunk_ind_mapping['index'] == i]
            chunk_name = tt['chunk_name'].unique()[0]
            if tt.shape[0] > work_query_batch_size:
                chunk_ind_tt = [tt.index[list(range(len(tt)))[i:i + work_query_batch_size]].values.tolist() for i in range(0, len(tt), work_query_batch_size)]
                chunk_ind_label_tt = [f'{chunk_name}_{x}' for x in range(len(chunk_ind_tt))]
            else:
                chunk_ind_tt = [tt.index.values.tolist()]
                chunk_ind_label_tt = [chunk_name]
            pkl_lab_tt = []
            for j, lab in enumerate(chunk_ind_label_tt):
                pkl_lab_tt.extend([lab]*len(chunk_ind_tt[j]))
            chunk_ind_mapping.loc[tt.index, 'pkl_lab'] = pkl_lab_tt
            chunk_ind.extend(chunk_ind_tt)
            chunk_ind_label.extend(chunk_ind_label_tt)
        if sum(range(len(reference_index))) != sum([inner for outer in chunk_ind for inner in outer]):
            raise ValueError(f'- error when splitting "reference_index" into "chunk_ind" - check total rows')
        if len(chunk_ind_label) + len(chunk_ind) + chunk_ind_mapping['pkl_lab'].nunique() != len(chunk_ind) * 3:
            raise ValueError(f'- error when splitting "reference_index" into "chunk_ind" - check length in "chunk_ind" and "pkl_lab"')
            
        # run queries
        query_log=pd.DataFrame()
        query_path_csv=os.path.join(sentiment_folder, '01_sentence_query_'+model_lab+'.csv')
        error_path_pkl=os.path.join(sentiment_folder, '99_error_log_'+model_lab+'.pkl')
        error_message=[]
        print('     *** error log saved into', error_path_pkl, '(even if code fails)\n')
        tokens_ind=0
        for i, ind in enumerate(chunk_ind):

            chunk_label=chunk_ind_label[i]
            rotate_tokens=tokens[tokens_ind % len(tokens)]
            headers={"Authorization": f"Bearer {rotate_tokens}"}
            
            message=f'Querying batch ({len(ind)} rows) {str(i + 1)} / {str(len(chunk_ind))} [Token:...{rotate_tokens[-2:]}]  last interaction: {datetime.datetime.now().strftime("%d/%m/%Y %H:%M:%S")}'
            print(message, end = '\r')
            
            chunck_path=os.path.join(query_folder, chunk_label+'.pkl').replace('|', '___')
            if not query_reload or not os.path.exists(chunck_path):

                ref_ind=chunk_ind_mapping.iloc[ind]['ref_index'].values.tolist()
                text_batch=np.array(sentence_chunks)[ind].tolist()

                start=timer()
                if model_ID not in custom_model_ID:
                    max_try=1
                    while max_try <= 2:
                        try:
                            query_out=query({"inputs": text_batch, "options": {"wait_for_model": True}},
                                            api_url=urljoin(api_url, model_ID), headers=headers)
                            if type(query_out)!=list and 'error' in query_out.keys():
                                query_out=pd.DataFrame({'Status': 'FAILED', 'Error': query_out['error'], 'ref_index': ref_ind})
                                error_message.append((chunk_label, 'query_out error', query_out['error']))
                            else:
                                query_out=format_results(query_out)
                                query_out.insert(0, 'ref_index', ref_ind)
                                query_out.insert(0, 'Error', '')
                                query_out.insert(0, 'Status', 'OK')
                                break
                        except Exception as e:
                            query_out=pd.DataFrame({'Status': 'FAILED', 'Error': e, 'ref_index': ref_ind})
                            error_message.append((chunk_label, 'try error', e))
                        max_try+=1
                elif model_ID == 'ai-lab/ESGify':
                    try:
                        to_model = tokenizer.batch_encode_plus(text_batch, add_special_tokens=True, max_length=max_length,
                                                               return_token_type_ids=False, padding="max_length", truncation=False,
                                                               return_attention_mask=True, return_tensors='pt')
                        query_out = model(**to_model)
                        query_out=format_results_local(query_out, pred_classes)
                        query_out.insert(0, 'ref_index', ref_ind)
                        query_out.insert(0, 'Error', '')
                        query_out.insert(0, 'Status', 'OK')
                    except Exception as e:
                        query_out=pd.DataFrame({'Status': 'FAILED', 'Error': e, 'ref_index': ref_ind})
                        error_message.append((chunk_label, 'try error', e))

                query_out.insert(0, 'pkl_lab', chunk_label)
                match_url=chunk_ind_mapping[chunk_ind_mapping['pkl_lab']==chunk_label].pop(uniqueID)
                query_out.insert(0, uniqueID, match_url.values)
                query_out['eval_time']=datetime.timedelta(seconds=round(timer()-start)).total_seconds()
                if len(query_out[query_out['Status']=='FAILED']) == 0:
                    query_out.to_pickle(chunck_path, protocol=-1)
            else:
                query_out=pd.read_pickle(chunck_path)

            query_log=pd.concat([query_log, query_out])
            if not query_reload:
                query_log.to_csv(query_path_csv, index=False, sep=';')
            tot_fail_batch=query_log[query_log['Status'] == 'FAILED']['pkl_lab'].nunique()
            tot_fail_rows=(query_log['Status'] == 'FAILED').sum()
            if tot_fail_rows > 0:
                joblib.dump(error_message, error_path_pkl, compress=('lzma', 3))
            print(message+f'  - total failed batch: {tot_fail_batch} ({tot_fail_rows} rows)      ', end = '\r')
            tokens_ind+=1

        query_log.to_csv(query_path_csv, index=False, sep=';')
        query_log.insert(0, 'Model', model_ID)
        display(query_log['Status'].value_counts().to_frame())
        if len(query_log[query_log['Status'] == 'FAILED']) > 0:
            print(colored('Try to run the code again with', 'black', 'on_light_grey'),
                  'query_reload='+colored('True', 'green',  attrs=['bold']))
        if len(os.listdir(query_folder)) != chunk_ind_mapping['pkl_lab'].nunique():
            cc=chunk_ind_mapping['pkl_lab'].nunique()
            extra_files=list(set(os.listdir(query_folder)) - set((chunk_ind_mapping['pkl_lab']+'.pkl').unique()))
            print(colored('total pickles mismatch:', 'black', 'on_light_grey'),
                 f'{len(os.listdir(query_folder))} in "query_folder" but {cc} expected\n   Extra files:')
            print('     ','\n      '.join(extra_files[:10]), '\n      ...' if cc > 10 else '')
        output[model_lab]=query_log

        tot_time=query_log.groupby('pkl_lab').first()['eval_time'].sum()
        print('\nDone in ', str(datetime.timedelta(seconds=round(tot_time))))
        query_path=os.path.join(sentiment_folder, '01_sentence_query_'+model_lab+'.pkl')
        query_log.to_pickle(query_path, protocol=-1)
        print('Data saved in', query_path)
        
    summary = pd.DataFrame()
    for k, v in output.items():
        row = v['Status'].value_counts().to_frame().rename(columns={'Status': 'Count'}).reset_index(names='Status')
        row.insert(0, 'Model', k)
        summary=pd.concat([summary, row])
    print('\n\n\n\n'+'#'*70+'\n')
    display(summary.groupby("Model", group_keys=True).apply(lambda x: x).drop(columns='Model'))
        
    return output


def evaluate_sentiment_dataset(mod, df, query_log, aggregate='max', display_sample=True, uniqueID='url', mapping_folder=''):

    df_ESGNone = None
    # aggregate by 'mean' or 'max'
    df_avg=df.drop(columns=['Model', 'pkl_lab', 'Status', 'Error', 'ref_index', 'max', 'eval_time'])
    df_avg=df_avg.groupby(uniqueID).agg({c: aggregate for c in df_avg.drop(columns=[uniqueID])}).reset_index()

    def apply_softmax(row):
        v = row.drop(uniqueID).astype('float')
        softmax = np.exp(v) / sum(np.exp(v))
        return pd.concat([pd.Series({uniqueID: row[uniqueID]}), softmax])
    def apply_normalize(row):
        v = row.drop(uniqueID).astype('float')
        norm = v / sum(v)
        return pd.concat([pd.Series({uniqueID: row[uniqueID]}), norm])
    if mod == 'ai-lab_ESGify':
        print(f'####   Evaluate: {mod}      with {aggregate}')
        class_map = pd.read_csv(os.path.join(mapping_folder, 'Mapping_ESGfy.csv'), sep=';')
        df_ESGNone = (pd.melt(df_avg, id_vars=[uniqueID], var_name='Class')
                     .merge(class_map, on='Class', how='left')
                     .drop(columns='Class')
                     .groupby([uniqueID, 'Category'], as_index=False).sum()
                     .pivot(index=uniqueID, columns='Category', values='value')
                     .reset_index()
                    )[[uniqueID, 'Environmental', 'Social', 'Governance', 'None']]
        df_ESGNone = df_ESGNone.apply(apply_normalize, axis=1)     # normalize
        #.sum(axis=1, numeric_only=True).min()  #  check row sums to 1
        df_ESGNone['ESGsubclass'] = df_avg.apply(lambda row: row.drop(uniqueID).to_dict(), axis=1)

    if mod == 'ESGBERT_EnvironmentalBERT-environmental':
        print(f'####   Evaluate: ESGBERT      with {aggregate}')
        df_avg_all=pd.DataFrame()
        for class_type in ['EnvironmentalBERT-environmental', 'SocialBERT-social', 'GovernanceBERT-governance']:
            df=query_log['ESGBERT_'+class_type]
            if aggregate == 'mean':
                    df_avg=df.drop(columns=['Model', 'pkl_lab', 'Status', 'Error', 'ref_index', 'max', 'eval_time']).groupby(uniqueID).mean().reset_index()
            if aggregate == 'max':
                df_avg=df.drop(columns=['Model', 'pkl_lab', 'Status', 'Error', 'ref_index', 'max', 'eval_time'])
                df_avg=df_avg.groupby(uniqueID).agg({'none': 'min', class_type.split('-')[1]: 'max'}).reset_index()
            df_avg = df_avg.apply(apply_normalize, axis=1)     # normalize
            df_avg['max_'+class_type.split('-')[1]]=df_avg.idxmax('columns', numeric_only=True)
            df_avg = df_avg.rename(columns={'none': 'none_'+class_type.split('-')[1]})
            if len(df_avg_all) == 0:
                df_avg_all = df_avg.copy()
            else:
                df_avg_all = df_avg_all.merge(df_avg, on=uniqueID, how='left')
        orig_cols = df_avg_all.columns.drop(uniqueID)
        def evaluate_ESG(row):
            t = row[['max_environmental', 'max_social', 'max_governance']].values#.tolist().remove('none')
            t = [i for i in t if i != 'none']
            if len(t) > 0:
                t1 = row[t].astype(float).sort_values(ascending=False)
                t1 = np.exp(t1) / sum(np.exp(t1))
                t1.index = [x.capitalize() for x in t1.index.values.tolist()]
            else:
                t1 = row[t]
                t1['None'] = 1
            return pd.concat([row, t1])
        df_ESGNone = df_avg_all.apply(evaluate_ESG, axis=1).fillna(0).drop(columns=orig_cols)
        for class_type in ['Environmental', 'Social', 'Governance']:
            if class_type not in df_ESGNone.columns:
                df_ESGNone[class_type] = 0
        df_ESGNone = df_ESGNone[[uniqueID, 'Environmental', 'Social', 'Governance', 'None']]
#         df_ESGNone[['Environmental', 'Social', 'Governance', 'None']]=df_ESGNone[['Environmental', 'Social', 'Governance', 'None']].astype(int)
        df_avg_all['ESGsubclass'] = df_avg_all[[col for col in df_avg_all if not col.startswith('max_')]].apply(lambda row: row.drop(uniqueID).to_dict(), axis=1)
        df_ESGNone = df_ESGNone.merge(df_avg_all[[uniqueID, 'ESGsubclass']], on=uniqueID, how='left')

    if mod == 'TrajanovRisto_bert-esg':     # here returns only 3 class with sentiment -1,0,1 (multiple values are allowed)
        print(f'####   Evaluate: {mod}      with {aggregate}')
        orig_cols = df_avg.columns.drop(uniqueID)
        def evaluate_sentiment(row):

            prob_thresh = 0.3
            for class_type in ['Environmental', 'Social', 'Governance']:
                row_t = row[[col for col in df if col.startswith(class_type)]]
                row_t = row_t / sum(row_t)    # normalize
                row_t = row_t[row_t > prob_thresh]
                out = 0
                if len(row_t) > 0:
                    max_class = row_t.index.values[row_t.values.argmax()]
                    if 'positive' in max_class.lower():
                        out = 1
                    elif 'negative' in max_class.lower():
                        out = -1
                row[class_type+'Sentiment'] = out
            return row.drop(orig_cols)

        df_ESGNone = df_avg.apply(evaluate_sentiment, axis=1)
        df_ESGNone['ESGSentimentsubclass'] = df_avg.apply(lambda row: row.drop(uniqueID).to_dict(), axis=1)

    if mod == 'yiyanghkust_finbert-tone':     # here returns only 1 class with sentiment -1,0,1
        print(f'####   Evaluate: {mod}      with {aggregate}')
        df_ESGNone = df_avg.copy()
        df_ESGNone = df_ESGNone.apply(apply_normalize, axis=1)     # normalize
        df_ESGNone['Sentiment']=df_ESGNone.idxmax('columns', numeric_only=True)
        conditions = [
            (df_ESGNone['Sentiment'] == 'Negative'),
            (df_ESGNone['Sentiment'] == 'Neutral'),
            (df_ESGNone['Sentiment'] == 'Positive')
            ]
        df_ESGNone['FinSentiment']=np.select(conditions, [-1, 0, 1])
        df_ESGNone.drop(columns=['Neutral', 'Positive', 'Negative', 'Sentiment'], inplace=True)
        df_ESGNone['FinSentimentsubclass'] = df_avg.apply(lambda row: row.drop(uniqueID).to_dict(), axis=1)

    if mod == 'yiyanghkust_finbert-esg':   # take 'ESGsubclass' from 'yiyanghkust_finbert-esg-9-categories'
        print(f'####   Evaluate: {mod}      with {aggregate}')
        df_ESGNone = df_avg.copy()
        df_ESGNone = df_ESGNone.apply(apply_normalize, axis=1)     # normalize
        df1=query_log['yiyanghkust_finbert-esg-9-categories']
        df_avg1=df1.drop(columns=['Model', 'pkl_lab', 'Status', 'Error', 'ref_index', 'max', 'eval_time'])
        df_avg1=df_avg1.groupby(uniqueID).agg({c: aggregate for c in df_avg1.drop(columns=[uniqueID])}).reset_index()
        df_avg1 = df_avg1.apply(apply_normalize, axis=1)     # normalize
        # class_map = pd.read_csv(os.path.join(mapping_folder, 'Mapping_finbert-esg-9.csv'), sep=';')
        # df_ESGNone = (pd.melt(df_avg1, id_vars=[uniqueID], var_name='Class')
        #              .merge(class_map, on='Class', how='left')
        #              .drop(columns='Class')
        #              .groupby([uniqueID, 'Category'], as_index=False).sum()
        #              .pivot(index=uniqueID, columns='Category', values='value')
        #              .reset_index()
        #             )[[uniqueID, 'Environmental', 'Social', 'Governance', 'None']]
        # df_ESGNone['ESGsubclass'] = df_avg.apply(lambda row: row.drop(uniqueID).to_dict(), axis=1)
        df_avg1['ESGsubclass'] = df_avg1.apply(lambda row: row.drop(uniqueID).to_dict(), axis=1)
        df_ESGNone = df_ESGNone.merge(df_avg1[[uniqueID, 'ESGsubclass']], on=uniqueID, how='left')
        df_ESGNone = df_ESGNone[[uniqueID, 'Environmental', 'Social', 'Governance', 'None', 'ESGsubclass']]

    if df_ESGNone is not None:
        if display_sample:
            display(df_ESGNone.head(2))
        dd=df_ESGNone[[x for x in df_ESGNone.columns if not re.search(uniqueID+'|subclass', x)]]
        if len(np.unique(dd.values)) < 4:
            sentiment_stat=(pd.melt(dd).groupby(['variable', 'value'], as_index=False)
                            .size()
                            .pivot(index='variable', columns='value', values='size')
                            .reset_index()
                            .set_index('variable'))
            sentiment_stat.columns.name=''
            sentiment_stat.index.name=''
            display(sentiment_stat.fillna(0).astype(int))
        else:
            display(dd.agg(['min','mean', 'max']).T)
        print("      Row sum range: {:5f} - {:5f}".format(df_ESGNone.sum(axis=1, numeric_only=True).min(), df_ESGNone.sum(axis=1, numeric_only=True).max()))
    
    return df_ESGNone


ESGwordlist= pd.read_csv('.\\Data and papers\\Momtaz_ESGdict.csv')
e = [x.replace('_',' ') for x in list(ESGwordlist['E'].dropna())]
s = [x.replace('_',' ') for x in list(ESGwordlist['S'].dropna())]
g = [x.replace('_',' ') for x in list(ESGwordlist['G'].dropna())]

def get_ngrams(s, n):
    '''
    tokenize an input text 
    source: https://albertauyeung.github.io/2018/06/03/generating-ngrams.html
    '''
    # Convert to lowercases
    s = s.lower()
    
    # Replace all none alphanumeric characters with spaces
    s = re.sub(r'[^a-zA-Z0-9\s]', ' ', s)
    
    # Break sentence in the token, remove empty tokens
    tokens = [token for token in s.split(" ") if token != ""]
    
    # Use the zip function to help us generate n-grams
    # Concatentate the tokens into ngrams and return
    ngrams = zip(*[tokens[i:] for i in range(n)])
    return [" ".join(ngram) for ngram in ngrams]


def cleaner(txt):    
    '''
    Clean the input text
    '''
    return clean(
        txt,
        fix_unicode=True,               # fix various unicode errors
        to_ascii=True,                  # transliterate to closest ASCII representation
        lower=True,                     # lowercase text
        no_line_breaks=True,           # fully strip line breaks as opposed to only normalizing them
        no_urls=True,                  # replace all URLs with a special token
        no_emails=True,                # replace all email addresses with a special token
        no_phone_numbers=True,         # replace all phone numbers with a special token
        no_numbers=True,                # replace all numbers with a special token
        no_digits=True,                 # replace all digits with a special token
        no_currency_symbols=True,      # replace all currency symbols with a special token
        no_punct=True,                 # remove punctuations
        replace_with_punct="",          # instead of removing punctuations you may replace them
        replace_with_url="",
        replace_with_email="",
        replace_with_phone_number="",
        replace_with_number="",
        replace_with_digit="",
        replace_with_currency_symbol="",
        lang="en"                       # set to 'de' for German special handling
        )


def ESG_Calculator(text):
    '''
    return the ESG scores for an input text.  MODIFIED with softmax
    https://github.com/sasi2400/sustainableentrepreneurship.org/blob/main/Notebooks/ESG%20score%20calculation.ipynb
    '''
    try:      
        text=re.sub('[\\n]','',text)
        text=get_ngrams(text,1)+get_ngrams(text,2)+get_ngrams(text,3)+get_ngrams(text,4)
        counts=[]
        counts= [text.count(x) for x in e]
        e_Freq=sum(counts) 
        e_Diversity = len([x for x in counts if x!=0])    
        counts= [text.count(x) for x in s]
        s_Freq=sum(counts) 
        s_Diversity = len([x for x in counts if x!=0])            
        counts= [text.count(x) for x in g]
        g_Freq=sum(counts) 
        g_Diversity = len([x for x in counts if x!=0])    
        esg_Diversity= e_Diversity/len(e) +s_Diversity/len(s) +g_Diversity/len(g)
        e_score = e_Diversity/len(e)
        s_score = s_Diversity/len(s)
        g_score = g_Diversity/len(g)
        none_score = 1 - esg_Diversity
        # apply softmax
        v = [e_score, s_score, g_score, none_score]
        v = np.exp(v) / sum(np.exp(v))
        return {'Environmental': v[0], 'Social': v[1], 'Governance': v[2], 'None': v[3]}
    except Exception as ex:
        print(ex)
        return {'Environmental': None, 'Social': None, 'Governance': None}
    
    
def scrape_coinmarketcap(row=None, add_row=None, skip_download=False, check_cookies=False,
                         chromedriver_path='', price_folder='', screenshot_folder=''):
    
    '''
    Scrape price series and link with Selenium.
    
    - row: row from df_list.iterrows()
    - add_row: if not None, the script will try to download again the information if status is not 'DOWNLOADED' or 'FOUND'
    - skip_download: if True skips price series download
    - check_cookies: if True, check for cookies tab and reject
    '''
    
    url=row['url']
    file_name=row['url2']
    ticker=row['ticker']
    
    previous_time=0
    status_download='SKIPPED'
    error_download=''
    website=''
    page_type='ERROR'
    error_website=''
    skip_website=False
    whitepaper=''
    error_whitepaper=''
    skip_whitepaper=False
    if add_row is not None:
        previous_time=add_row['TotTimeSec'][0]
        if add_row['PriceSeriesStatus'][0] not in ['DOWNLOADED', 'UNTRACKED', 'PAGE_NOT_AVAILABLE']:
            skip_download=False
        else:
            status_download='DOWNLOADED'
        if add_row['WebsiteStatus'][0] == 'FOUND':
            status_website='FOUND'
            website=add_row['Website'][0]
            page_type=add_row['PageType'][0]
            skip_website=True
        if add_row['WhitepaperStatus'][0] == 'FOUND':
            status_whitepaper='FOUND'
            whitepaper=add_row['Whitepaper'][0]
            skip_whitepaper=True
        

    # create temp folder for csv download
    temp_download_folder=os.path.join(os.getcwd(), 'temp_folder_' + file_name)
    if os.path.exists(temp_download_folder):
        shutil.rmtree(temp_download_folder)
    os.makedirs(temp_download_folder)

    expected_downloaded_file=os.path.join(temp_download_folder, ticker+'_ALL_graph_coinmarketcap.csv')
    expected_downloaded_file_alt=os.path.join(temp_download_folder, ticker+'_All_graph_coinmarketcap.csv')
    save_csv_path=os.path.join(price_folder, file_name + '.csv')

    # open page
    start=timer()
    driver=get_chromedriver(chromedriver_path=chromedriver_path, download_folder=temp_download_folder)
    driver.get(url)
    cookie_list=['//*[@id="onetrust-reject-all-handler"]', '//*[@id="onetrust-accept-btn-handler"]',
                 '/html/body/div[5]/div[2]/div/div[1]/div/div[2]/div/button[2]',
                 '/html/body/div[5]/div[2]/div/div[1]/div/div[2]/div/button[1]']
    if check_cookies:
        for pp in cookie_list:
            try:              # close cookies
                cookie=WebDriverWait(driver, 2).until(
                    EC.presence_of_element_located((By.XPATH, pp)))
                cookie.click()
                break
            except:
                pass
    driver.maximize_window()
    
    # check if page not available
    if any([x in driver.page_source for x in ['something went wrong', "Sorry, we couldn't find"]]):
        status_download='PAGE_NOT_AVAILABLE'
        skip_download=True
        skip_website=True
        status_website='ERROR'
        skip_whitepaper=True
        status_whitepaper='ERROR'
    
    # download price series
    if not skip_download:
        
        # check if Market data is untracked
        if any([x in driver.page_source for x in ['Market data is untracked', 'This project is an untracked listing']]):
            status_download='UNTRACKED'
            
        else:
            try:
                # interact with chart and press download button
                for scroll in [0, 300, 600, 900, 1200]:
                    try:
                        driver.execute_script(f"window.scrollTo(0, {scroll});")
                        allButton=WebDriverWait(driver, 3).until(
                #             EC.presence_of_element_located((By.XPATH, '(//ul[@class="react-tabs__tab-list"])[1]//li[text()="ALL"]')))
                            EC.presence_of_element_located((By.XPATH, '(//ul[@class="react-tabs__tab-list"])[1]//li[text()="All"] | (//ul[@class="react-tabs__tab-list"])[1]//li[text()="ALL"]')))

                        allButton.click()
                        break
                    except:
                        pass
                    if check_cookies:
                        for pp in cookie_list:
                            try:              # close cookies
                                cookie=WebDriverWait(driver, 2).until(
                                    EC.presence_of_element_located((By.XPATH, pp)))
                                cookie.click()
                                break
                            except:
                                pass
                exportButton=allButton.find_element(By.XPATH, "(//li//div[contains(@class,'custom-button-inner')])[2]")
                actions=ActionChains(driver)
                actions.move_to_element(exportButton).pause(2).click().perform()
                downloadAsCsvButton=WebDriverWait(driver, 10).until(
                    EC.visibility_of_element_located((By.XPATH, '//button//div[text()="Download price history (.csv)"]')))
                downloadAsCsvButton.click()
                
                # check download and move to folder
                for time_left in Timeout(20):
                    time.sleep(0.3)
                    folder_content=os.listdir(temp_download_folder)
                    if len(folder_content) > 0:
                        status_download='FILE_DOWNLOADED_BUT_ERROR_IN_STORING'
                    if os.path.exists(expected_downloaded_file):
                        shutil.copy(expected_downloaded_file, save_csv_path)                       
                    if os.path.exists(expected_downloaded_file_alt):
                        shutil.copy(expected_downloaded_file_alt, save_csv_path)
                    try:
                        dd=pd.read_csv(save_csv_path, sep=';')
                        if 'name' in dd.columns:
                            dd.drop(columns='name').to_csv(save_csv_path, index=False, sep=';')
                        status_download='DOWNLOADED'
                        break
                    except:
                        pass                        
            except Exception as e:
                error_download=str(e)
                status_download='ERROR'

    if status_download != 'DOWNLOADED':
        save_csv_path=None

    # get website url
    if not skip_website:
        try:
            websiteButton=driver.find_element(By.XPATH, '//div[text()="Website"]')
            actions=ActionChains(driver)
            actions.move_to_element(websiteButton).perform()
            urlElement=WebDriverWait(driver, 15).until(
                EC.visibility_of_element_located((By.CSS_SELECTOR, 'div[class="tippy-content"] span')))
            website=urlElement.text
            status_website='FOUND'
            page_type='NORMAL'
        except Exception as e:
            error_website=str(e)
            status_website='ERROR'
        # it seems there are two version of the same page, in the alternative one the Website button behaves differently,
        # it shows the url as button text. Try to use xpath instead
        if page_type == 'ERROR':
            try:
                websiteButton=driver.find_element(By.XPATH, '//*[@id="__next"]/div/div[1]/div[2]/div/div[1]/div[2]/div/div[1]/div[3]/div/div[1]/ul/li[1]/a')
                website=websiteButton.get_attribute('href')
                status_website='FOUND'
                page_type='ALTERNATIVE'
            except Exception as e:
                error_website=str(e)
                status_website='ERROR'

    # get whitepaper url
    if not skip_whitepaper:
        try:
            whitepaperButton = driver.find_element(By.XPATH, '//a[text()="Whitepaper"]')
            whitepaper=whitepaperButton.get_attribute('href')
            status_whitepaper='FOUND'
        except Exception as e:
            error_whitepaper=str(e)
            status_whitepaper='ERROR'
        
    # take page screenshot for debug
    screen_path=None
#     if page_type == 'ALTERNATIVE' or status_website == 'ERROR':
#         screen_path=os.path.join(screenshot_folder, file_name + '.png')
#         driver.save_screenshot(screen_path)

    # close page and remove temp folder
    driver.close()
    try:
        shutil.rmtree(temp_download_folder)
    except:
        pass

    add_row=pd.DataFrame({'url': url, 'PriceSeriesStatus': status_download, 'PriceSeriesPath': save_csv_path,
                          'PriceSeriesError': error_download, 'PageType': page_type, 'WebsiteStatus': status_website,
                          'Website': website, 'WebsiteError': error_website,  'WhitepaperStatus': status_whitepaper,
                          'Whitepaper': whitepaper, 'WhitepaperError': error_whitepaper, 'ScreenPath': screen_path,
                         'TotTimeSec': previous_time + datetime.timedelta(seconds=round(timer()-start)).total_seconds()}, index=[0])
    
    return add_row


def navigate_html(nested: dict, key='', value=''):
    for k, v in nested.items():
#         if k == key and v == value:
        if key in v:
            yield {k: v}
        elif isinstance(v, list):
            for d in v:
                if isinstance(d, dict):
                    yield from navigate_html(d, key, value)
                    
                    
def scrape_icomarketcap_link_only(row):
    
    '''
    Scrape only links, with request instead of Selenium.
    
    - row: row from df_list.iterrows()
    '''
    
    url=row['url']
    file_name=row['url2']
    warnings=''
    error=''
    error_dict=''
    
    start=timer()
    try:
        webpage = requests.get(url)
        soup = BeautifulSoup(webpage.content, "html.parser")
        d=convert(soup)
        if any([x in soup.prettify() for x in ['something went wrong', "Sorry, we couldn't find"]]):
            error='PAGE_NOT_AVAILABLE'
        else:            
            # find string with "urls"
            s=list(navigate_html(d, 'urls'))
            if len(s) > 1:
                warnings='len(s) > 1'
                for sub_s in s:
                    if '"urls"' in str(sub_s):
                        s=[sub_s]
            s=str(s[0])
            # extract string and convert to dictionary
            start_ind=s.find('"urls":')+7
            end_ind=s.find('}', start_ind)+1
            error_dict=s[start_ind:end_ind]
            d=json.loads(error_dict)
    except Exception as e:
        error=str(e)
    
    add_row=pd.DataFrame({'url': url, 'file_name': file_name, 'data': [d], 'warnings': warnings, 'error': error,
                          'error_dict': error_dict,
                          'TotTimeSec': datetime.timedelta(seconds=round(timer()-start)).total_seconds()})
    
    return add_row

def bytesToKey(salt, password):
    data = b''
    tmp = b''
    while len(data) < 48:
        md5 = MD5.new()
        md5.update(tmp + password + salt)
        tmp = md5.digest()
        data += tmp
    return data

def decrypt_CryptoTotem(decryptdict, password = b'mycrypt'):
    # https://github.com/meetio/cryptojs-aes
    # https://stackoverflow.com/questions/78186654/how-to-decode-url-that-is-encrypted-by-javascript-function   --- see comments
    
    ciphertext = base64.b64decode(decryptdict['ct'])
    salt = bytes.fromhex(decryptdict['s'])
    keyIv = bytesToKey(salt, password)
    key = keyIv[:32]
    iv = keyIv[32:] # from key derivation
    cipher = AES.new(key, AES.MODE_CBC, iv)
    decryptedstring = unpad(cipher.decrypt(ciphertext), 16)
    
    return decryptedstring.decode('utf8').replace('\/', '/').replace('"', '')

def call_API_CoinMarketCap(url, API_KEY, parameters):
    
    # https://coinmarketcap.com/api/documentation
    
    headers = {
      'Accepts': 'application/json',
      'X-CMC_PRO_API_KEY': API_KEY,
    }
    
    session = Session()
    session.headers.update(headers)

    err=None
    try:
      response = session.get(url, params=parameters)
      data = json.loads(response.text)
    except (ConnectionError, Timeout, TooManyRedirects) as e:
      err = e
    
    return data, err


def display_side_by_side(df, n_cols):
    
    step=round(len(df) / n_cols + 0.5)
    html_str=''
    for start in range(0, len(df), step):
        html_str+='<th style="text-align:center"><td style="vertical-align:top">'
        html_str+=df.iloc[start:start+step].to_html().replace('table','table style="display:inline"')
        html_str+='</td></th>'
    display_html(html_str,raw=True)