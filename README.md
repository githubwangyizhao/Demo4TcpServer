## 使用语言
  * Erlang

## 描述
  * 基于OTP原则构建的TCP服务器

## 参考来源
  *	 https://www.cnblogs.com/ribavnu/p/3409823.html
  
## 测试
  * 启动TCP服务:
     运行 Demo4Tcp\tcp_server\compile.cmd 文件
     运行 Demo4Tcp\tcp_server\run.cmd 文件
    
  * 客户端:
     cmd
     werl
     c(test_wsgate).
    
     S = test_wsgate:connect(false).
     test_wsgate:request(S, <<"Hello World !!">>").
     test_wsgat:close(S).
     
     test_wsgate:connect().
