// pblock starter
module pblock (input               clk,
               input        [ 4:0] state,
               input        [ 6:0] t,
               input        [31:0] n,
               input        [31:0] mem_read_data,
               input        [31:0] k1,
               output logic [31:0] hout);

logic [31:0] w[16];
logic [31:0] A, B, C, D, E, F, G, H;
	logic [31:0] temp[16]; //sixteen bc number of nonces
logic [31:0] h[8];
logic [31:0] h2[8];


parameter int p2[16] = '{
    32'h00000000,32'h00000000,32'h00000000,32'h80000000,
    32'h00000000,32'h00000000,32'h00000000,32'h00000000,
    32'h00000000,32'h00000000,32'h00000000,32'h00000000,
    32'h00000000,32'h00000000,32'h00000280,32'h00000000
};

parameter int p3[16] = '{
    32'h00000000,32'h00000000,32'h00000000,32'h00000000,
    32'h00000000,32'h00000000,32'h00000000,32'h80000000,
    32'h00000000,32'h00000000,32'h00000000,32'h00000000,
    32'h00000000,32'h00000000,32'h00000100,32'h00000000
};

function logic [31:0] rightrotate(input [31:0] x, input  [7:0] r);
  begin
	  rightrotate = (x >> r) | (x << (32-r)); //ADDED THIS
  end
endfunction

	
	
logic [31:0] t1, t2;

assign t1 = temp;
assign t2 = 
	
	//is this the sort of thing we need to do??
	
	/*
	function logic [255:0] sha_operation(input logic [31:0] A, B, C, D, E, F, G, temp);
  		logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
  		begin
    		S1 = rightrotate(E, 6) ^ rightrotate(E, 11) ^ rightrotate(E, 25);
    		ch = (E & F) ^ ((~E) & G);
    		t1 = S1 + ch + temp;
    		S0 = rightrotate(A, 2) ^ rightrotate(A, 13) ^ rightrotate(A, 22);
    		maj = (A & B) ^ (A & C) ^ (B & C);
    		t2 = S0 + maj;
    		sha_operation = {t1 + t2, A, B, C, D + t1, E, F, G};
  		end
  	endfunction
  */
	
	

assign hout = h[0];

logic [31:0] h1[8];

assign h1[0] = h[0] + a;
assign h1[1] = h[1] + b;
assign h1[2] = h[2] + c;
assign h1[3] = h[3] + d;
assign h1[4] = h[4] + e;
assign h1[5] = h[5] + f;
assign h1[6] = h[6] + g;
assign h1[7] = h[7] + h;

logic [31:0] wt;
	
	//is this the sort of thing we need to do??
	/*
	  function logic [31:0] wt(input logic [3:0] n);
    		logic [31:0] s0, s1;
	
		 s0 = rightrotate(w[n][1], 7) ^ rightrotate(w[n][1], 18) ^ (w[n][1]>>3);
		 s1 = rightrotate(w[n][14], 17) ^ rightrotate(w[n][14], 19) ^ (w[n][14]>>10);
		 wt = w[n][0] + s0+ w[n][9] + s1;      
  	  endfunction
  */

// A-H, temp
always_ff @(posedge clk) 
  if (!(t[6] && t[0])) begin // t<65
    if (state[4]) begin // COMPUTE1, COMPUTE2, COMPUTE3
	   temp <= 
	   {A, B, ... } <= 
    end 
    else begin
	   temp <=  w[15] +k1 +
	   {A, B, ... } <= {h[0],h[1],...};
    end
  end

// W ARRAY
always_ff @(posedge clk) begin
  for (int m = 0; m < 15; m++) w[m] <= w[m+1];
  wt <= w[1]  + (rightrotate(w[2],   7) ^ rightrotate(w[2],  18) ^ (w[2] >> 3)) +
        w[10] + (rightrotate(w[15], 17) ^ rightrotate(w[15], 19) ^ (w[15]>>10));
  if (state[4]) begin // COMPUTE1, COMPUTE2, COMPUTE3
    if (t<15) begin
      case (state[1:0])
        0: // COMPUTE1
			 w[15] <=    // come from mem
        1: begin // COMPUTE2	w[15] <= ...
           if (t<2)
		   w[15] <= mem_read_data; //FILLED THIS IN
           else if (t == 2)
			  w[15] <= n;
           else
			  w[15] <= p2[t];
           end
        default: begin
          if (t<7) begin
		    w[15] <=  h2[0];
            for (int m = 0; m < 7; m++) h2[m] <=
          end
          else
            w[15] <= p3[t];
        end
      endcase
    end 
	else 	 // if t>14
       w[15] <= wt;

      if (!state[1])  // not COMPUTE3
            {h2[0], h2[1], h2[2], h2[3], h2[4], h2[5], h2[6], h2[7]} 
      else if (state[4:1]==4'b0101) begin // PREP31, PREP32
		w[15] <= 
        for (int m = 0; m < 7; m++) h2[m] <= 
      end 
    else 
		w[15] <=     // Fetch more data
end

// H ARRAY
always_ff @(posedge clk) begin
  if (state[4] && state[1] && t[6] && t[0])  // COMPUTE3 && t>64

  else if (state[3]) begin 
    if (state[2:0]==...) begin 
      h[0] <= 32'h6a09e667;
      h[1] <= 32'hbb67ae85;
      h[2] <= 32'h3c6ef372;
      h[3] <= 32'ha54ff53a;
      h[4] <= 32'h510e527f;
      h[5] <= 32'h9b05688c;
      h[6] <= 32'h1f83d9ab;
      h[7] <= 32'h5be0cd19;
    end 
    else if (state[2:0]==...)  
	  {h[0] ,,, } <= {h1[0], ...};
  end
end
endmodule
