//---------------------------------------------------------------------------

// This software is Copyright (c) 2021 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Model.Constants;

interface

const
  TAB_CHAR = #9;
  ANIM_DURATION = 0.25;
  ACTIVITY_NAME_DELIM = 'lbl';
  sClassNamePrefix = 'T';
  dActivitiesCount = 11;
  dCommonUserSettingsBlockSignature = Integer($69696969);

  fPortraitModeToolBarPercetage = 6.5;
  fLandscapeModeToolBarPercetage = 11;

  // milliseconds.
  dLoopWaitForRealign = 10;

  sActivitySignInName = 'SignIn';
  sActivityHomeName = 'Home';
  sActivityMenuName = 'Menu';
  sActivitySignUp = 'SignUp';
  sActivitySettings = 'Settings';
  sActivityContacts = 'ContactsList';
  sActivityNewForm = 'NewForm';
  sActivityAbout = 'About';
  sActivityPrivacy = 'PrivacyPolicy';
  sActivityTermsOfUse = 'TermsOfUse';
  sActivityData = 'Data';
  sActivityProfile = 'Profile';
  sActivityClassNameSuffix = 'Frame';

  sLightThemeResName = 'Light';
  sDarkThemeResName = 'Dark';

  sModelDataName = 'ModelData';
  sModelDataClassName = Concat(sClassNamePrefix, sModelDataName);

  sUnitTestAppPostfix = '_tests';

  sLabelStyleTextResName = 'text';
  sEmail = 'Email';
  sInvalidEmailFormat = 'Invalid email format';
  sGrantStoragePermissionsMsgDlgText = 'Grant read\write storage permissions?';
  sCancelStoragePermissisonsText = Concat('Storage permissions denied!', #10, 'User settings were not loaded\saved propertly!');

  sDBSampleDataRectDisabledCaption = 'DB Access';
  sDataSampleDataRectDisabledValue = '[disabled]';

  sContactsNotSupported = 'Contacts list is not supported by Windows Platform';
  sPermissionsDenied = 'Premissions denied';
  sLoadingContacts = 'Loading contacts...';

  sEmailValidationRegexExpr = '^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]*[a-zA-Z0-9]+$';

  // List of all activities
  arrActivitiesList: array [0 .. dActivitiesCount - 1] of string =
    (sActivityProfile, sActivityContacts, sActivityData, sActivitySettings,
    sActivityAbout, sActivityNewForm, sActivityTermsOfUse, sActivityPrivacy,
    sActivitySignInName, sActivitySignUp, sActivityHomeName);

  // Activities which do not shown in Main Menu
  sHiddenMenuActivities = Concat(sActivityHomeName);

  SHOWPASS_IMG_OPACITY_DIV = 10;
  SHOWPASS_IMG_OPACITY_MUL = 4;
  SHOWPASS_IMG_OPACITY_VALUE = 0.1;

  CONFIG_FILENAME = 'config.bin';

  // Test user name from employee.gdb database.
  DEBUG_DB_USERNAME_1  = 'Glen';

  // Android theming.
  MATERIAL_UI_GREY_50  = $fffafafa;
  MATERIAL_UI_GREY_100 = $fff5f5f5;
  MATERIAL_UI_GREY_200 = $ffeeeeee;
  MATERIAL_UI_GREY_400 = $ffbdbdbd;

  // SVG Icons.
  MATERIAL_UI_MENU = 'M3,18 L21,18 L21,16 L3,16 L3,18 Z M3,13 L21,13 L21,11 L3,11 L3,13 Z M3,6 L3,8 L21,8 L21,6 L3,6 Z ';

  // DB routines.
  SQL_CMD_PARAM_DELIMITER = ':';
  CUST_NO_PARAM = 'CUST_NO';
  USER_NAME_PARAM = 'USER_NAME';
  sFilterbyFirstName = 'FIRST_NAME=''';
  sSingleQuoteChar = string('''');

  // ViewModel data layer SQL scripts.
  // We use these SQL for demo purposes.
  SQLCMD_GetCustNoByUserName = 'SELECT CUST_NO FROM CUSTOMER WHERE CONTACT_FIRST=' + SQL_CMD_PARAM_DELIMITER + USER_NAME_PARAM;
  SQLCMD_GetCustNoByUserName_FIELDNAME = 'CUST_NO';
  SQLCMD_GetTotalSumSales = 'SELECT SUM(TOTAL_VALUE) FROM SALES WHERE CUST_NO=' + SQL_CMD_PARAM_DELIMITER + CUST_NO_PARAM;
  SQLCMD_GetTotalSumSales_FIELDNAME = 'SUM';
  SQLCMD_GetAddressLine1 = 'SELECT ADDRESS_LINE1 FROM CUSTOMER WHERE CONTACT_FIRST=' + SQL_CMD_PARAM_DELIMITER + USER_NAME_PARAM;
  SQLCMD_GetAddressLine1_FIELDNAME = 'ADDRESS_LINE1';

  // IBLite demo dabatase file name.
  sDBName = 'employee.gdb';
  sDBStoragePathWindows = 'Res\db';
  sDBStoragePathMaskiOS = '%s%s%s';

  // String routines.
  sNotDefined = '[not defined]';
  sSpaceChar = ' ';

  // System messages.
  sAdressBookAccessError = 'You cannot access Address Book: ';
  sAdressBookNotSupported = Concat(sPermissionsDenied, sSpaceChar, 'or', sSpaceChar, 'this platform does not support the Address Book service');
  sUseSignInError = 'SignIn has failed. Email or Password is invalid!';
  sIBLiteErrorPrefix = 'IBLite Error: ';

implementation

end.
