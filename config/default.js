/**
 *  Default Config loads alongside environment specifc
 *  config files in config root folder. any values that
 *  are specific to the environment can be redeclared and
 *  then will override the defined values in the default
 *  config file
 *
 *  @author DJ Hayden <dj.hayden@stablekernel.com>
 */
module.exports = {
  mongo: {
    host: 'localhost',
    name: 'prizm'
  },

  env: {
    port: 3000
  },

  social: {
    facebook: {
      client_id: '1408826952716972',
      client_secret: '772f449b10c95a10a2a9a866339e5f90',
      client_token: '01e689dd2090d202b540c937515e1913',
      app_token: '1408826952716972|772f449b10c95a10a2a9a866339e5f90',
      base_uri: 'https://graph.facebook.com/v2.0',
      code_uri: 'https://graph.facebook.com/v2.0/oauth/code?',
      token_uri: 'https://graph.facebook.com/v2.0/oauth/access_token?',
      callback_uri: 'https://https://ec2-54-200-41-62.us-west-2.compute.amazonaws.com/callback'
    },
    twitter: {
      consumer_key: 'MzIoqUFCk7BYUNpCNxtGuhuLu',
      consumer_secret: 'yGhuwPvSljoVJoD4il2qtHZG0q4hWlXC87Mcdly0pxaFrMHEaf',
      dev_user_access_token: '2349321242-qfXdDvyKPWASbXydkbxIQfmHsYWKh8Bi6fVPAiw',
      dev_user_access_token_secret: 'SWjMYzLS7AzDfnJ1grb4ApYzZEHeSoojcRv2YtohthivB',
      dev_user_id: '2349321242',
      callback_uri: 'https://ec2-54-200-41-62.us-west-2.compute.amazonaws.com/callback'
    },
    google: {
      client_id: '657032544324-5m844imcarep80ibnqu7fp2r1aoo0m04.apps.googleusercontent.com',
      client_secret: '6Dx8PGIFQ9xzY0sxw3rl9Mq3',
      authorize_uri: 'https://www.googleapis.com/plus/v1/people/me?access_token=',
      auth_uri: 'https://accounts.google.com/o/oauth2/auth',
      token_uri: 'https://accounts.google.com/o/oauth2/token',
      callback_uri: 'https://ec2-54-200-41-62.us-west-2.compute.amazonaws.com/callback',
      realm: ''
    }
  },
  amazon: {
    smtp: {
      server: 'email-smtp.us-west-2.amazonaws.com',
      port: '465',
      tls: true,
      user: 'AKIAIPXRAQMGM7DXIJ6Q',
      pass: 'AhDzEadbTzKb70+JNW+7zeGxF9+iW0v/FsJc6dwe83Ni'
    }
  },
  base_uri: 'https://ec2-54-186-28-238.us-west-2.compute.amazonaws.com',
  mailchimp: {
    client_id: '514495820552',
    client_secret: '6b1ef1bcf668302af0252cc4afa1d6cc'
  },
  mandrill: {
    client_secret: 'SHzM16s9ZKmo-5bbQCZXfA'
  },
  push_server: 'http://ec2-54-186-223-100.us-west-2.compute.amazonaws.com/push',
  app_name: 'Prizm Dev'
};
