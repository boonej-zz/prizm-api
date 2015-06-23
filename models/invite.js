var mongoose = require('mongoose');
var ObjectId      = mongoose.Schema.Types.ObjectId;

var inviteSchema = new mongoose.Schema({
  address       : {type: String, required: true, index: true},
  status        : {type: String, default: 'unsent'},
  user          : {type: ObjectId, ref: 'User', required: false},
  organization  : {type: ObjectId, ref: 'Organization', index: true},
  create_date   : {type: Date},
  modify_date   : {type: Date},
  code          : {type: String},
  group         : {type: ObjectId, ref: 'Group', required: false}
});

inviteSchema.pre('save', function(next){
  if (!this.create_date){
    this.create_date = Date.now();
  }
  this.modify_date = Date.now();
  next();
});

mongoose.model('Invite', inviteSchema);
