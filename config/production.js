module.exports = {
  base_uri: 'PRIZM-FE-PRD-1163268161.us-west-2.elb.amazonaws.com',
  app_name: 'Prizm Production',
  mongo: {
  host: 'prizm:vuxar6facUth@72.31.38.6:27017,172.31.35.42:27017',     
//  host: 'prizm:vuxar6facUth@54.213.111.108:27017',
    name: 'prizm'
  },
  social: {
    google: {
              callback_uri: 'https://prizm-fe-prd-1163268161.us-west-2.elb.amazonaws.com/callback'
            }
  }
};
