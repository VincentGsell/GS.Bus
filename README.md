[![](https://tokei.rs/b1/github/VincentGsell/GS.Bus?category=code)](https://github.com//VincentGsell/GS.Bus)
[![](https://tokei.rs/b1/github/VincentGsell/GS.Bus?category=files)](https://github.com//VincentGsell/GS.Bus)
[![](https://tokei.rs/b1/github/VincentGsell/GS.Bus?category=lines)](https://github.com//VincentGsell/GS.Bus)
[![](https://tokei.rs/b1/github/VincentGsell/GS.Bus?category=blanks)](https://github.com//VincentGsell/GS.Bus)
[![](https://tokei.rs/b1/github/VincentGsell/GS.Bus?category=comments)](https://github.com//VincentGsell/GS.Bus)

# GS.Bus : *InterThread* communication Application Bus for FPC/Delphi

- History
	- 20220711 : Summer fixes and other
		- cleaning useless demo
		- enhancement for easiest usage.
	- Since 2019 : many changes ;)
  
  the unit GS.Bus owned a complete and easy to use application bus system.
  
- main features : 
  - Allowed to communicate by message throught app, same thread or not, between unit, module, form, or whatever.
  - *Efficient* bus with Channels management (publish/subscribe on topics and Queues)
  - Preserve send sequence and distribute messages in a balanced manner (no contention)
  - Thread Safe, **inter thread** communication : You can communicate efficently between thread, with no compromise or pain.
  - "synchronise solution Free" : Get incoming messages events *always* in your thread context, *without* synchro call.
  - Nice sides features : such as in-memory KeyValue pseudo DB : nice to shared data within an app.
  - Basic statistics capabilities.
  - Used *extensively* in many projects, and it is a GS.GRID pillars.
  
  ![Alt text](/../master/Ressources/BusSchema.png?raw=true "")
  
- Dependancy : 
  - [GS.Core](https://github.com/VincentGsell/GS.Core)
  
- Show me the code !

  - short previous docs : 
    - Messages are "just" bytes buffer.
    - Sending is as simple as "myBus.Send(MyMessage, 'A nice topic');
      - This method is crafted to be fast.
      - There are mirror methods, such as Recv and SendAndRecv to facilitate rpc call.
    - There are a key function, to call from everywhere you want : BusProcessMessages([client1, client2, ...])
      - this method will delivers messages from the subscribe's client you choose, where you want.
      - This method allow, in fact, to choose in *which thread context* you will process the incoming messages
    - Bus dispatch message is done in backround thread.
	  	  
  - here is an exemple app : 
  
  this exemple show how to use the main feature of the bus : A subscription to a topic, and message send and delivery.
  Basically, we have 2 buttons : One simply send a message on topic "Test channel", the other create a thread.
  This thread send e message on "Test channel" too, wait 500ms, and loop until app finished.
  
  finally, we put in a 1ms timer the "famous" BusProcessMessage(), and this one will collect the messages previously dispatched by the bus.
  Each time there a message, the event defined in application start will be trigered.
  
  ![Alt text](/../master/Ressources/MinimalBusExemple.png?raw=true "")
  
	  



# Demo
  
  **GS.Bus**
  
  *ProjectMinimalBus* is a very simple exemple : It send and receive message, in the main GUI thread from itself and from another thread.
  Begin with that to undestand fully how the stuff work.
  
  ![Alt text](/../master/Ressources/MinimalBusExempleApp.png?raw=true "")
   
  the next demo bellow allowed to send messages, with many subscribtion, and multithread sender.
  The app will show hits if incming data, and show how heavy loading could be managed into an app.
  
  ![Alt text](/../master/Ressources/busbench.png?raw=true "")
 
  
  this demo show the Key/Value features.
  Technically, it is entirely based uppon Bus message communication. It Show how transform the bus into local, in app, interthread RPC system.
  
  ![Alt text](/../master/Ressources/busbench_kv.png?raw=true "")  
  
    
  **GS.MemCached**
  
  This demo show a complete KeyValue with "basic" file persistence : If you plan to use it pro, please, consider to replace the persitence layer. We have several one, if you wish.
  
  ![Alt text](/../master/Ressources/memcached.png?raw=true "")

    
  
