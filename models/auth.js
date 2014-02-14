/**
 * Auth Models
 *
 * @author DJ Hayden<dj.hayden@stablekernel.com>
 */
var mongoose  = require('mongoose')
  , uuid      = require('node-uuid');

Date.prototype.addMinutes = function(minutes){
  return this + minutes*60000;
};

var clientApplicationSchema = new mongoose.Schema({
  name                : {type: String, required: true},
  description         : {type: String, required: true},
  redirect_uri        : {type: String, required: false},
  client_id           : {type: String},
  client_secret       : {type: String}
});

clientApplicationSchema.pre('save', function(next){
  if (! this.client_secret && ! this.client_id){
        this.client_id = uuid.v4();
        this.client_secret = uuid.v4();
    }
  next();
});

var codeSchema = new mongoose.Schema({
  client_id           : {type: String, required: true},
  code                : {type: String},
  redirect_uri        : {type: String},
  scope               : [],
  state               : {type: String},
  date_created        : {type: Date}
});

codeSchema.pre('save', function(next){
  this.code = uuid.v4();
  this.date_created = Date.now();
  next();
});

var tokenSchema = new mongoose.Schema({
  access_token        : {type: String, required: false},
  grant_type          : {type: String, required: true},
  token_type          : {type: String, required: false},
  expires_in          : {type: Number, required: false},
  refresh_token       : {type: String, required: false},
  date_created        : {type: Date, required: false},
  date_expires        : {type: Date, required: false},
  client_application  : {type: mongoose.Schema.Types.ObjectId, ref: 'ClientApplication', required: true},
  code                : {type: String, required: true},
  scope               : []
});

tokenSchema.methods.expiresIn = function(){
  return (this.date_expires - Date.now())/1000;
}

tokenSchema.methods.cleanJSON = function(){
  var cleaned = {
      access_token  :   this.access_token,
      expires_in    :   this.expiresIn(),
      token_type    :   this.token_type
    }
  if (this.grant_type == 'authorization_code' ||
           this.grant_type == 'refresh_token') {
      cleaned.refresh_token = this.refresh_token;
    }
  return cleaned;
};

tokenSchema.pre('save', function(next){
  var validFor = 10 * 60 * 1000,
      dateNow = Date.now();

  this.access_token = uuid.v4();
  this.token_type = 'Bearer';
  this.date_created = dateNow;
  this.date_expires = dateNow + validFor;
  if (this.grant_type == 'authorization_code'
         || this.grant_type == 'refresh_token') {
             this.refresh_token = uuid.v4();
           }
  next();
});

exports.ClientApplication = mongoose.model('ClientApplication', clientApplicationSchema);
exports.Code = mongoose.model('Code', codeSchema);
exports.Token = mongoose.model('Token', tokenSchema);
