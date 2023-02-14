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
from pdf2image import convert_from_path
import pytesseract
from PIL import Image
from tika import parser



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


def pdf_to_text(file_path='', tesseract_path='', lang='eng'):
    
    # open pdf
    parsed_pdf = parser.from_file(file_path)

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


def scrape_info_icomarks(url='', chromedriver_path='', skip_social=False):
    
    '''
    - skip_social: if True skip social users' timeseries download (takes time and uses WebDriver)
    '''
    
    add_row=pd.DataFrame()
    
    #### request page
    page = requests.get(url)
    soup = BeautifulSoup(page.content, 'html.parser')


    #### page screenshot date
    add_row['url']=[url]
    try:
        tag = soup.find_all('div', class_="swimm-panel-bottom__links", recursive=True)
        conv_dict = convert(tag)
        add_row['PageScreenshot']=[conv_dict['div'][0]['#text']]
    except:
        pass


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
            series_dict=get_social_series(url=url, tot_series=social_df.shape[0], chromedriver_path=chromedriver_path)
            series_status = 'DOWNLOADED' if len(series_dict) != 0 else 'DOWNLOAD_ERROR'
        else:
            series_dict={}
            series_status='DOWNLOAD_SKIPPED'

        add_row['SocialWithRating']=social_df.shape[0]
        add_row['SocialSeriesStatus']=series_status
        add_row['SocialBlock']=[[{'stats': social_df, 'timeseries': series_dict}]]
        
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
    #                 # todo: si possono calcolare medie, dev stand, ecc
    #         else:
    #             track_social=None

    #         add_row['TrackedSocial']=track_social

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