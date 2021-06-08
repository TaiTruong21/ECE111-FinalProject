/* pblock starter
  Hint 1: see flow chart in Piazza @303
  Hint 2: test bench does the same operations you need to do
*/
module pblock (input               clk,
               input        [ 4:0] state,
               input        [ 6:0] t,
               input        [31:0] n,
               input        [31:0] mem_read_data,
               input        [31:0] k1,
               output logic [31:0] hout);

logic [31:0] w[16];
logic [31:0] A, B, C, D, E, F, G, H;
logic [31:0] temp;
logic [31:0] h[8];
logic [31:0] h1[8];
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

endfunction

logic [31:0] t1, t2;

/*S1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
    ch = (e & f) ^ ((~e) & g);
    t1 = h + S1 + ch + k[t] + w;
    S0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
    maj = (a & b) ^ (a & c) ^ (b & c);
    t2 = S0 + maj; 
	sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};*/

assign t1 = H + (rightrotate(E,6)^ rightrotate(E, 11) ^ rightrotate(E, 25)) + ((E&F) ^ ((~E)&G) )+ k1[t] + temp ;
assign t2 = (rightrotate(A, 2) ^ rightrotate(A, 13) ^ rightrotate(A, 22)) +((A&B)^(A&C)^(B&C));

assign hout = h[0];

assign h1[0] = h[0] + A;
assign h1[1] = h[1] + B;
assign h1[2] = h[2] + C;
assign h1[3] = h[3] + D;
assign h1[4] = h[4] + E;
assign h1[5] = h[5] + F;
assign h1[6] = h[6] + G;
assign h1[7] = h[7] + H;

logic [31:0] wt;

// A-H, temp
always_ff @(posedge clk) 
  if (!(t[6] && t[0])) begin // t<65
    if (state[4]) begin // COMPUTE1, COMPUTE2, COMPUTE3
	   temp <= w[15] + k1 + G;
	   {A, B, C, D, E, F, G, H} <= {t1+t2, A, B, C, D+t1, E, F, G}; // concatenation of t1+t2, A:C, D+t1, E:G
    end 
    else begin
	   temp <= w[15] + k1 + h[7];
	   {A, B, C, D, E, F, G, H} <= {h[0],h[1],h[2],h[3],h[4],h[5],h[6],h[7]};
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
			 w[15] <=  mem_read_data;  // hint: comes from memory
        1: begin // COMPUTE2	w[15] <= ...
           if (t<2)
			  w[15] <= mem_read_data;  // hint: comes from memory
           else if (t == 2)
			  w[15] <= n;
           else
			  w[15] <= p2[t];
           end
        default: begin
          if (t<7) begin
		    w[15] <=  h2[0];
            for (int m = 0; m < 7; m++) h2[m] <= h2[n+1]; // move h2[15:1] down to h2[14:0]
          end
          else
            w[15] <= p3[t];
        end
      endcase
    end 
	else 	 // if t>14
       w[15] <= wt;

      if (!state[1])  // not COMPUTE3
            {h2[0],h2[1],h2[2],h2[3],h2[4],h2[5],h2[6],h2[7]} <= {h1[0],h1[1],h1[2],h1[3],h1[4],h1[5],h1[6],h1[7]};
	  end
      else if (state[4:1]==4'b0101) begin // PREP31, PREP32
		w[15] <= h2[0];					// as above in t<7 case
        for (int m = 0; m < 7; m++) h2[m] <= h2[n+1];    //as above 
      end 
    else 
		w[15] <= mem_read_data;    // Fetch more data
end

// H ARRAY
always_ff @(posedge clk) begin
  if (state[4] && state[1] && t[6] && t[0])  // COMPUTE3 && t>64
	  {h[0],h[1],h[2],h[3],h[4],h[5],h[6],h[7]} <= {h1[0],h1[1],h1[2],h1[3],h1[4],h1[5],h1[6],h1[7]};
  else if (state[3]) begin 
    if (state[2:0]==3'b100 || state[2:0]==3'b010) begin  //PREP12 or PREP31
      h[0] <= 32'h6a09e667;
      h[1] <= 32'hbb67ae85;
      h[2] <= 32'h3c6ef372;
      h[3] <= 32'ha54ff53a;
      h[4] <= 32'h510e527f;
      h[5] <= 32'h9b05688c;
      h[6] <= 32'h1f83d9ab;
      h[7] <= 32'h5be0cd19;
    end 
    else if (state[2:0]==3'b000)  // at PREP21 set h<=h1
	  {h[0],h[1],h[2],h[3],h[4],h[5],h[6],h[7]} <= {h1[0],h1[1],h1[2],h1[3],h1[4],h1[5],h1[6],h1[7]};
  end
end
endmodule
