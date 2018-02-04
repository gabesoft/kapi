// updateDates('lastPostDate');
// updateDates('lastReadDate');

function updateDates(dateField) {
  var ops = [];
  var query = {};

  query[dateField] = { '$exists':  true,  '$type' :  2 };

  var feeds = db.feeds.find(query);

  feeds.forEach(function(feed) {
    var updateValue = {};
    updateValue[dateField] = new ISODate(feed[dateField]);

    ops.push({
      updateOne : {
        filter : { _id:  feed._id },
        update : { '$set':  updateValue }
      }
    });

    if (ops.length  ===  500) {
      db.feeds.bulkWrite(ops);
      ops = [];
    }
  });

  if (ops.length > 0) {
    db.feeds.bulkWrite(ops);
  }
}

