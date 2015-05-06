var mongoose = require('mongoose');
var ObjectId      = mongoose.Schema.Types.ObjectId;

var muteSchema = new mongoose.Schema({
  user          : {type: ObjectId, ref: 'User'},
  create_date   : {type: Date},
  revoke_date   : {type: Date}
});

muteSchema.statics.selectFields = function(type){
  return ['user', 'organization', 'group', 'create_date', 'revoke_date', 'active'];
}

muteSchema.statics.canResolve = function(){
  return [
    {organization: {identifier: '_id',  model: 'Organization'}},
    {user: {identifier: '_id', model: 'User'}},
    {group: {identifier: '_id', model: 'Group'}}
  ];
}

muteSchema.methods.format = function(type, add_fields, callback){
  return {
    user: this.user,
    organization: this.organization,
    group: this.group,
    create_date: this.create_date,
    revoke_date: this.revoke_date,
    active: this.active
  }
}

muteSchema.pre('save', function(next){
  if (! this.create_date){
    this.create_date = Date.now();
  }
  next();
});

