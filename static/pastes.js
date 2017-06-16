// stuff to handle pastes in the index

function render (data) {
  const t = document.querySelector('#paste-item');
  t.content.querySelector('li').setAttribute('id', 'paste-' + data.id);
  const p = t.content.querySelectorAll('p');
  p[0].querySelector('b').textContent = data.title;
  p[1].textContent = data.text;
  return document.importNode(t.content, true);
}

function load () {
  const request = new XMLHttpRequest();
  request.open('GET', '/api/pastes/', true);

  request.onload = function() {
    if (request.status >= 200 && request.status < 400) {
      // Success!
      const pastesList = document.querySelector('#pastes-list');
      const pastesData = JSON.parse(request.responseText);

      console.log('loading pastes', pastesData);
      pastesList.innerHTML = '';
      pastesData.reverse().forEach((paste) =>
        pastesList.appendChild(render(paste)));
    } else {
      // We reached our target server, but it returned an error
      console.log('Failed to load pastes!');
    }
  };

  request.onerror = function() {
    console.log('I Fail!');
  };

  request.send();
}

function addNew (data) {
  document.querySelector('#pastes-list').prepend(render(data));
}

function remove (id) {
  document.querySelector('#paste-' + id).remove();
}

function replace (data) {
  remove(data.id);
  addNew(data);
}

const actionHandlers = {
  create: addNew,
  update: replace,
  delete: remove
};

function connectSocket() {
  const ws = new WebSocket('ws://localhost:8080/websockets');

  ws.onmessage = function (message) {
    const data = JSON.parse(message.data);
    console.log('Received socket message', message, data);
    actionHandlers[data.action](data.data);
  };
}

load();
connectSocket();
