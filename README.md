## Precedence Climbing

* https://www.bilibili.com/video/BV1jf4y1n7Mb

* https://mp.weixin.qq.com/s/MuuaROoH7gI0wYVypEOoWw

```
expr(min_prec)
	result = factor()
	while min_prec <= cur_operator_prec
		prec,assoc = cur_operator_prec,cur_operator_assoc
		if assoc is left:
			next_min_prec = prec + 1
		else:
			next_min_prec = prec
		rhs = expr(next_min_prec)
		result = compute operator(result,rhs)
	return result
```