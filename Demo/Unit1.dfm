object Form1: TForm1
  Width = 206
  Height = 164
  object WebButton1: TWebButton
    Left = 15
    Top = 20
    Width = 96
    Height = 25
    Caption = 'WebButton1'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = WebButton1Click
  end
  object TContINI: TTextCont
    Lines.Strings = (
      '[platformio]'
      'default_envs = megaatmega2560'
      'lib_dir=C:\Work\Arduino\Libs'
      ''
      '[common]'
      
        'build_flags = -D LOGOUTPUT=SERIAL MACADDR={0xA8,0x61,0x0A,0xAE,0' +
        'x17,0xC8}'
      ';  -D MaxNumberTimers=5'
      ''
      '[env:uno]'
      'platform = atmelavr'
      'board = uno'
      'framework = arduino'
      'monitor_speed = 128000'
      'monitor_port = COM4'
      'lib_ldf_mode = deep'
      '; lib_deps = ${common.lib_deps}'
      'debug_tool = simavr'
      'build_flags = ${common.build_flags} -D ARDUINO_UNO'
      ''
      '[env:megaatmega2560]'
      'platform = atmelavr'
      'board = megaatmega2560'
      'framework = arduino'
      'monitor_speed = 128000'
      'monitor_port = COM3'
      'lib_ldf_mode = deep'
      '; lib_deps = ${common.lib_deps}'
      'debug_tool = simavr'
      'build_flags =  ${common.build_flags} -D ARDUINO_MEGA'
      'mega=true'
      'versionDate=2018-10-07T18:22:47.128')
    Width = 437
    Height = 662
    Left = 95
    Top = 60
  end
  object TContYAML: TTextCont
    Lines.Strings = (
      'gain: false'
      'steam: false'
      'anybody:'
      '  place: true'
      '  cloth: -457558039.4780614'
      '  factor: true'
      '  forward: structure'
      '  rays: street'
      'discuss: lesson'
      'turn:'
      '  sentence: false'
      '  audience: said'
      '  clothing: 453613887'
      '  mother: 1481028039.4780614'
      '  matter:'
      '    began: -1161561004.5577965'
      '    introduced:'
      '      feed: talk'
      '      advice:'
      '        charge: -1608666226.335727'
      '        sort: airplane'
      '        possible: degree'
      '        coming: settlers'
      '        location: -611341393'
      '      clock: 407184349'
      '      eleven: announced'
      '      hall: true'
      '    political: -1804812630'
      '    chest: our'
      '    pipe: cowboy')
    Width = 437
    Height = 662
    Left = 25
    Top = 60
  end
end
