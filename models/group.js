var mongoose = require('mongoose');
var ObjectId      = mongoose.Schema.Types.ObjectId;

var groupSchema = new mongoose.Schema({
  name          : {type: String, required: true},
  description   : {type: String, default: ''},
  organization  : {type: ObjectId, ref: 'Organization', required: true},
  create_date   : {type: Date} 
});

groupSchema.pre('save', function(next){
  if (!this.create_date) {
    this.create_date = Date.now();
  }
  next();
});

mongoose.model('Group', groupSchema);
