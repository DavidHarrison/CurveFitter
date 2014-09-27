/* file: hash_table.c */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hash_table.h"
#include "memcheck.h"

void memoryFail(void);
void print_linked_list(node *list);

/*** Hash function. ***/

int hash(char *s)
{
    char c;
    int i, sum;
    c = s[0];
    sum = 0;
    for (i = 1; c != '\0'; i++)
    {
        sum += (int) c;
        c = s[i];
    }
    return sum % 128;
}


/*** Linked list utilities. ***/

/* Create a single node. */
node *create_node(char *key, void *value)
{
    node *n;
    n = (node *) malloc(sizeof(node));
    if (n == NULL) memoryFail();
    n->key = key;
    n->value = value;
    n->next = NULL;
    return n;
}


/* Free all the nodes of a linked list. */
void free_list(node *list)
{
    node *n, *next_node;
    n = list;
    while (n != NULL)
    {
        next_node = n->next;
        free(n->key);
        free(n);
        n = next_node;
    }
}


/*** Hash table utilities. ***/

/* Create a new hash table. */
hash_table *create_hash_table()
{
    hash_table *ht;
    int i;
    ht = (hash_table *) malloc(sizeof(hash_table));
    if (ht == NULL) memoryFail();
    ht->slot = malloc(NSLOTS * sizeof(node *));
    if (ht->slot == NULL) memoryFail();
    for (i = 0; i < NSLOTS; i++)
    {
        ht->slot[i] = NULL;
    }
    return ht;
}


/* Free a hash table. */
void free_hash_table(hash_table *ht)
{
    int i;
    for(i = 0; i < NSLOTS; i++)
    {
        /* printf("freeing hash %d\n", i); */
        free_list(ht->slot[i]);
    }
    free(ht->slot);
    free(ht);
}


/*
 * Look for a key in the hash table.  Return 0 if not found.
 * If it is found return the associated value.
 */
int get_value(hash_table *ht, char *key)
{
    node *n;
    n = ht->slot[hash(key)];
    while (n != NULL)
    {
        if (strcmp(n->key, key) == 0)
        {
            return n->value;
        }
        n = n->next;
    }
    /* if the value is not found */
    return 0;
}


/*
 * Set the value stored at a key.  If the key is not in the table,
 * create a new node and set the value to 'value'.  Note that this
 * function alters the hash table that was passed to it.
 */
void set_value(hash_table *ht, char *key, void *value)
{
    node *n;
    n = ht->slot[hash(key)];
    while (n != NULL)
    {
        if (strcmp(n->key, key) == 0)
        {
            n->value = value;
            free(key);
            return;
        }
        if (n->next == NULL)
        {
            n->next = create_node(key, value);
            return;
        }
        n = n->next;
    }
    /* if no value is present at the hash */
    ht->slot[hash(key)] = create_node(key, value);
}


/* Print out the contents of the hash table as key/value pairs. */
void print_hash_table(hash_table *ht)
{
    int i;
    for (i = 0; i < NSLOTS; i++)
    {
        print_linked_list(ht->slot[i]);
    }
}

void print_linked_list(node *list)
{
    node *n;
    n = list;
    while (n != NULL)
    {
        printf("%s %d\n", n->key, n->value);
        n = n->next;
    }
}

void memoryFail(void)
{
    printf("Failed to allocate memory; exiting\n");
    exit(1);
}
