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
    name: 'prism'
  },

  env: {
    port: 3000
  },

  social: {
    facebook: {
      client_id: '601764173232668',
      client_secret: '23ce3259b183e6b3138b44a96a640bd7',
      client_token: '39a3c31b9d88efa03ae2ccd739385bdc',
      app_token: '601764173232668|KLtQOeNF2Ge1pBPN08fbEuylc8w',
      base_uri: 'https://graph.facebook.com',
      code_uri: 'https://graph.facebook.com/oauth/code?',
      token_uri: 'https://graph.facebook.com/oauth/access_token?',
      callback_uri: 'https://https://ec2-54-200-41-62.us-west-2.compute.amazonaws.com/callback'
    },
    twitter: {
      consumer_key: 'Ru65wMMNzljgbdZxie6okg',
      consumer_secret: 'sJHdOEwTXQDO2y7nEjeHRdt8gX0TUhirOSNk32o',
      dev_user_access_token: '2349321242-qfXdDvyKPWASbXydkbxIQfmHsYWKh8Bi6fVPAiw',
      dev_user_access_token_secret: 'SWjMYzLS7AzDfnJ1grb4ApYzZEHeSoojcRv2YtohthivB',
      dev_user_id: '2349321242',
      callback_uri: 'https://ec2-54-200-41-62.us-west-2.compute.amazonaws.com/callback'
    },
    google: {
      client_id: '308658825260.apps.googleusercontent.com',
      client_secret: '4CFksKO4jeQhTeRlvpqeXrKF',
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
