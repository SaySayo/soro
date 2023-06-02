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

              if (msg.type === "user") {
              chatSender.textContent = `${msg.sender}: `;
              chatMsg.textContent = msg.message;
              chatWrapper.appendChild(chatSender);
              chatWrapper.appendChild(chatMsg);
              chatSender.classList.add("chat-sender");
              chatMsg.classList.add("chat-msg");
              li.appendChild(chatWrapper);
              li.appendChild(chatFeedback);
              chatList.appendChild(li);
              } else if (msg.type === "system") {
                chatMsg.textContent = msg.message;
                chatMsg.classList.add("system-msg");
                li.appendChild(chatMsg);
                chatList.appendChild(li);
              }
        }

        const renderChatHistory = (history) => {
          createAndAddChat({type: "system", message: "You're welcome!"});
          history.forEach((msg) => {
            if(msg.type === "system") {
              createAndAddChat(msg);
              } else
            if(msg.type === "user") {
              createAndAddChat(msg);
              }
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

        /* socket.onopen = function () {
          let joinMessage =
            ["SystemMessage", ["JoinMessage", "message"]];
          socket.send(JSON.stringify(joinMessage));
        }; */

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

          let msg = ["UserMessage",{sender : username.innerHTML, message}]

          createAndAddChat(msg);
          console.log(msg);
          socket.send(JSON.stringify(msg));
          messageElem.value = "";
          return false;
        };
      </script>
    </body>
  </html>

type user_chat_entry = {sender : string; message : string} [@@deriving yojson]

type user_chat_details = user_chat_entry list [@@deriving yojson]

type system_chat_entry = JoinMessage of string | LeaveMessage of string [@@deriving yojson]

type chat_entry = UserMessage of user_chat_entry | SystemMessage of system_chat_entry [@@deriving yojson]

let load_chat_history () = 
  match Yojson.Safe.from_file "chat_history.json" with
  | json -> json |> user_chat_details_of_yojson
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
  match entry with
  | UserMessage user_entry -> (chat_details := user_entry :: !chat_details;
      let chat_details_json = !chat_details |> yojson_of_user_chat_details in
      Yojson.Safe.to_file "chat_history.json" chat_details_json)
  | SystemMessage _ -> ()

let handle_client client =
let client_id = track client in
let%lwt () = 
  let join_msg = SystemMessage (JoinMessage "New user joined") in
  send (Yojson.Safe.to_string (yojson_of_chat_entry join_msg)) in
let%lwt () =
let history_message =
  `Assoc [
    ("type", `String "chat_history");
    ("history", !chat_details |> yojson_of_user_chat_details)
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
      (match message_object with
        | UserMessage msg ->
            let%lwt () =
              let entry = UserMessage { sender = msg.sender; message = msg.message } in
              add_chat_entry entry;
              send message
            in
            loop ()
        | SystemMessage (JoinMessage username) ->
            let join_message = SystemMessage (JoinMessage username) in
            let%lwt () = send (Yojson.Safe.to_string (yojson_of_chat_entry join_message)) in
            loop ()
        | _ -> loop ()) 
  | None ->
      forget client_id;
      let%lwt () = 
      let leave_msg = SystemMessage (LeaveMessage "User left") in
      send (Yojson.Safe.to_string (yojson_of_chat_entry leave_msg)) in
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