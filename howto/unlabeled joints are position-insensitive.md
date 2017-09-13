# Unlabeled joints are insensitive to position

The unary relationship "#word _" (for instance, "#maybe _") is different from the unary relatinship "_ #word". However, if there is no label attached to the joint, as in "# _" and "_ #", then they are indistinguishable. 

```
> extractTplt $ InsRel eo [""] [Absent,InsLeaf $ Word "char"]
Tplt ["",""]
> extractTplt $ InsRel eo ["dd"] [Absent,InsLeaf $ Word "char"]
Tplt ["dd",""]
> extractTplt $ InsRel eo ["dd"] [InsLeaf $ Word "char", Absent]
Tplt ["","dd"]
```

If that becomes a problem, I'll extend the Tplt constructor to distinguish between the absent joint and the unlabeled joint. But I'm not sure I'll even be using the unlabeled joint, except in tests.
