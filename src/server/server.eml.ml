let home =
  <html lang="en">
    <head>
      <meta charset="UTF-8" />
      <meta http-equiv="X-UA-Compatible" content="IE=edge" />
      <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css" />
      <link rel="stylesheet" href="/static/main.css">
      <title>Chat App</title>
    </head>
    <body class="bg-gray-100">
      <div class="container mx-auto px-4 py-8">
        <h1 class="text-3xl font-bold text-center">Welcome to Soro Chat</h1>
        <p class="text-center mt-2">Pick up from where you left off...</p>
        <div class="bg-white rounded-lg shadow-lg mt-8">
          <div class="p-4 border-b border-gray-200 flex justify-center">      
              <div class="text-lg font-bold text-gray-800">
                <span class="mr-2">Username:</span>
                <span class="inline-block" id="username-title" contenteditable="true" title="Click to edit">Click To Edit</span>
              </div>  
          </div>
          <div class="chat-room p-4">
            <ol class="chat-list space-y-4" id="chatList">
            </ol>
          </div>
          <div class="chatInput p-4">
            <form id="chatForm">
              <input id="chatInput" type="text" placeholder="Type here..." class="w-1/2 px-4 py-2 rounded border border-gray-300 focus:outline-none focus:ring-2 focus:ring-blue-500">
              <button type="submit" class="icons plane">
                <i class="fa fa-send"></i>
              </button>
            </form>
          </div>
        </div>
      </div>
      <script>
        let username = document.getElementById("username-title");
        let chatList = document.getElementById("chatList");
        let messageElem = document.getElementById("chatInput");

        const createAndAddChat = (msg) => {
          let li = document.createElement("li");
              let chatWrapper = document.createElement("span");
              let chatSender = document.createElement("span");
              let chatMsg = document.createElement("span");
              let chatFeedback = document.createElement("i");
              chatSender.textContent = `${msg.sender}: `;
              chatMsg.textContent = msg.message;
              chatWrapper.appendChild(chatSender);
              chatWrapper.appendChild(chatMsg);
              chatSender.classList.add("chat-sender");
              chatMsg.classList.add("chat-msg");
              li.appendChild(chatWrapper);
              li.appendChild(chatFeedback);
              chatList.appendChild(li);
        }

        const renderChatHistory = (history) => {
          createAndAddChat({sender: "Bot", message: "You're welcome!"});
          history.forEach((msg) => {
            createAndAddChat(msg);
          });
          
        };

        let socket = new WebSocket("ws://" + window.location.host + "/websocket");

        socket.onmessage = function (event) {
          console.log(event.data);
          let messages = document.querySelectorAll(".chat-msg");
          let exist = false;
          let receivedData = JSON.parse(event.data);
          messages.forEach((message, i) => {
            if (message.textContent.toLowerCase() === receivedData.message.toLowerCase()) {
              message.parentElement.nextElementSibling.classList.add(
                "fa",
                "fa-check",
                "sent"
              );
              exist = true;
            }
          });

          if (exist === false) {
            createAndAddChat(receivedData);
          }
        };

        socket.onopen = function () {
          console.log("WebSocket connection established");
        };

        socket.onmessage = function (event) {
          let receivedData = JSON.parse(event.data);
          console.log(receivedData);
          if (receivedData.type === "chat_history") {
            renderChatHistory(receivedData.history);
          } else {
            let messages = document.querySelectorAll(".chat-msg");
            let exist = false;
            messages.forEach((message, i) => {
              console.log(message.innerHTML, receivedData.message);
              if (message.innerHTML.toLowerCase() === receivedData.message.toLowerCase()) {
                message.parentElement.nextElementSibling.classList.add(
                  "fa",
                  "fa-check",
                  "sent"
                );
                exist = true;
              }
              console.log(message.innerHTML, receivedData.message);
            });
            if (exist === false) {
              createAndAddChat(receivedData);
            }
          }
        };

        document.querySelector("form").onsubmit = function (e) {
          e.preventDefault();
          let message = messageElem.value;
          if (socket.readyState != WebSocket.OPEN)
            return false;
          if (!message)
            return false;

          let msg = {
            sender: username.textContent,
            message
          }

          createAndAddChat(msg);

          socket.send(JSON.stringify(msg));
          messageElem.value = "";
          return false;
        };
      </script>
    </body>
  </html>

type chat_entry = {sender : string; message : string} [@@deriving yojson]

type chat_details = chat_entry list [@@deriving yojson]

let load_chat_history () = 
  match Yojson.Safe.from_file "chat_history.json" with
  | json -> json |> chat_details_of_yojson
  | exception Sys_error _ -> []
  | exception Yojson.Json_error _ -> 
    Dream.log "chat_history.json file is not valid thus input is being ignored"; 
    []

let chat_details = ref (load_chat_history ())

let clients : (int, Dream.websocket) Hashtbl.t = Hashtbl.create 7

let track = 
  let last_client_id = ref 0 in
  fun websocket -> 
    last_client_id := !last_client_id + 1;
    Hashtbl.replace clients !last_client_id websocket;
    !last_client_id
  
let forget client_id =
  Hashtbl.remove clients client_id

let send message =
  Hashtbl.to_seq_values clients
  |> List.of_seq
  |> Lwt_list.iter_p (fun client -> Dream.send client message)

let add_chat_entry entry = 
  chat_details := entry :: !chat_details;
  let chat_details_json = !chat_details |> yojson_of_chat_details in
  Yojson.Safe.to_file "chat_history.json" chat_details_json

let handle_client client =
let client_id = track client in
let%lwt () =
let history_message =
  `Assoc [
    ("type", `String "chat_history");
    ("history", !chat_details |> yojson_of_chat_details)
  ] in
  let () =
   Dream.log "Sending chat history: %s" (Yojson.Safe.to_string history_message) in
  Dream.send client (Yojson.Safe.to_string history_message)
in
let rec loop () = 
  match%lwt Dream.receive client with
  | Some message -> 
      Dream.log "Server received a message: %s" message;
      let message_object =
        message
        |> Yojson.Safe.from_string
        |> chat_entry_of_yojson
      in
      let%lwt () =
        let entry = {sender = message_object.sender; message = message_object.message} in 
        add_chat_entry entry;
        send message
      in
      loop ()
  | None ->
      forget client_id;
      Dream.close_websocket client
in
loop ()

let () =
Dream.run 
@@ Dream.logger 
@@ Dream.router [
  Dream.get "/" (fun _ -> Dream.html home);

  Dream.get "/websocket"
    (fun _ -> Dream.websocket handle_client);
  
  Dream.get "/static/**" (Dream.static "_build/default/static");
  ]